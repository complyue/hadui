{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE QuasiQuotes #-}

-- | hadui runtime
module HaduiRT
    ( MsgToUI(..)
    , uiLog
    , uiClearLog
    , uiComm
    , haduiExecStmt
    , haduiExecGhc
    , haduiPubServer
    )
where

import           RIO
import qualified RIO.Text                      as T
import qualified RIO.Text.Lazy                 as TL
import           RIO.FilePath

import qualified Data.ByteString.Builder       as SB

import           Data.Dynamic                   ( Dynamic(..) )

import           Data.Yaml.Aeson

import qualified GHC
import qualified HscTypes                      as GHC
import qualified ErrUtils                      as GHC
import qualified Panic                         as GHC
import qualified GhcMonad                      as GHC
import qualified GhcPlugins                    as GHC

import           System.IO.Unsafe
import qualified System.Directory              as D
import           System.Posix.Types
import           System.Posix.Process
import           System.Posix.IO
import           UnliftIO.Process
import           UnliftIO.Concurrent

import           Network.Socket
import qualified Network.WebSockets            as WS

import qualified Data.Aeson                    as A
import           Data.Aeson.QQ                  ( aesonQQ )

import qualified Snap.Core                     as Snap
import qualified Snap.Http.Server              as Snap
import qualified Snap.Util.FileServe           as Snap

import           HaduiCfg
import           HaduiMonad

import qualified Paths_hadui                   as Meta


-- | Communicate a json command back to the front UI, via current
-- WebSocket connection in context.
--
-- The schema/protocol of json based communication is facilitated
-- by `wsc.js`, where the command form:
--  `{"type":"call", "method": "<name>", "args": <...>}`
-- is open at each hadui application's discrepancy.
--
-- A hadui aware stack project implements websocket methods in 
-- `hadui/hadui-custom.js` under the root directory.
uiComm :: A.ToJSON a => a -> UIO ()
uiComm jsonCmd = do
    uio <- ask
    let txtPayload = A.encode jsonCmd
        txtPacket  = WS.Text txtPayload Nothing
    liftIO $ withMVar (haduiGIL uio) $ \case
        Nothing ->
            runUIO uio
                $  logError
                $  display
                $  "No ws in context for comm of: "
                <> (Utf8Builder $ SB.lazyByteString txtPayload)
        Just wsc -> WS.sendDataMessage wsc txtPacket
    return ()


data MsgToUI = TextMsg Text
    | DetailedMsg Text TheDetails
    | ErrorMsg Text
    | DetailedErrorMsg Text TheDetails
    | RawHtmlMsg Text -- this and more TBD
type TheDetails = Text

uiLog :: MsgToUI -> UIO ()
uiLog msg = case msg of
    TextMsg msgText -> uiComm [aesonQQ|{
"type": "msg"
, "msgText": #{msgText}
}|]
    DetailedMsg msgText msgDetails -> uiComm [aesonQQ|{
"type": "msg"
, "msgText": #{msgText}
, "msgDetails": #{msgDetails}
}|]
    ErrorMsg errText -> uiComm [aesonQQ|{
"type": "msg"
, "msgText": #{errText}
, "msgType": "err-msg"
}|]
    DetailedErrorMsg errText errDetails -> uiComm [aesonQQ|{
"type": "msg"
, "msgText": #{errText}
, "msgType": "err-msg"
, "msgDetails": #{errDetails}
}|]
    -- more TBD
    _ -> pure ()

uiClearLog :: UIO ()
uiClearLog = uiComm [aesonQQ|{
"type": "clear"
}|]


-- | Execute an GHC action with GHC errors caught and displayed in web UI
haduiExecGhc :: GHC.Ghc () -> UIO ()
haduiExecGhc ghcAction = do
    uio <- ask
    let ghcSession = haduiGhcSession uio
        logGhcExc exc = do
            let errDetails = tshow exc
            runUIO uio $ uiLog $ DetailedErrorMsg "GHC error: " errDetails
        logSrcError err = do
            dynFlags <- GHC.getSessionDynFlags
            -- TODO use colors to highlight the error information
            let errDetails = T.pack $ unlines $ map
                    (GHC.showSDoc dynFlags)
                    (GHC.pprErrMsgBagWithLoc $ GHC.srcErrorMessages err)
            runUIO uio $ uiLog $ DetailedErrorMsg "Source error: " errDetails
    liftIO
        $ (flip GHC.reflectGhc) ghcSession
        $ GHC.handleGhcException logGhcExc
        $ GHC.handleSourceError logSrcError --
                                ghcAction

haduiExecStmt :: Text -> UIO ()
haduiExecStmt stmt = haduiExecGhc $ do
    -- have 'GHC.execLineNumber' start from 0 so line numbers in error
    -- report will match original source.
    _execResult <- GHC.execStmt
        ("mustUIO $\n"  -- hint required type with this line prepended
                       ++ T.unpack stmt)
        GHC.execOptions { GHC.execSourceFile = "<hadui-adhoc>"
                        , GHC.execLineNumber = 0
                        }
    -- todo say sth about the result to front UI ?
    return ()


-- serve a ws until it's disconnected
haduiServeWS :: WS.Connection -> UserInterfaceOutput -> IO ()
haduiServeWS wsc uio =
    let
        servOnePkt = do
            pkt <- WS.receiveDataMessage wsc
            case pkt of
                (WS.Text _bytes (Just pktText)) ->
                    runUIO uio
                        $ withHaduiFront wsc
                        $ haduiExecStmt
                        $ TL.toStrict pktText
                (WS.Binary _bytes) -> do
                    runUIO uio
                        $ logError "hadui received binary packet from ws ?!"
                    WS.sendCloseCode wsc 1003 ("?!?" :: Text)
                _ -> do
                    runUIO uio $ logError
                        "hadui received unknown packet from ws ?!"
                    WS.sendCloseCode wsc 1003 ("?!?" :: Text)
            -- anyway we should continue receiving, even after close request sent, 
            -- we are expected to process a CloseRequest ctrl message from peer.
            servOnePkt
    in  catch servOnePkt $ \exc -> case exc of
            WS.CloseRequest closeCode closeReason
                | closeCode == 1000 || closeCode == 1001 -> runUIO uio
                $ logDebug "hadui ws closed normally."
                | otherwise -> do
                    runUIO uio
                        $  logError
                        $  display
                        $  "hadui ws closed with code "
                        <> display closeCode
                        <> " and reason ["
                        <> (Utf8Builder $ SB.lazyByteString closeReason)
                        <> "]"
                    -- yet still try to receive ctrl msg back from peer
                    haduiServeWS wsc uio
            WS.ConnectionClosed ->
                runUIO uio $ logDebug $ "hadui ws disconnected."
            _ -> runUIO uio $ logError "hadui unexpected ws error"


haduiPubServer :: GHC.Ghc ()
haduiPubServer = do
    dataDir <- liftIO Meta.getDataDir
    let haduiResRoot = dataDir </> "web"
    unlessM (liftIO $ D.doesDirectoryExist haduiResRoot)
        $ error "hadui web resource directory missing ?!"

    uio <- initUIO
    let cfg = haduiConfig uio

    runUIO uio $ do
        logDebug
            $  "hadui using resource dir: ["
            <> fromString haduiResRoot
            <> "]"
        logInfo
            $  "hadui publishing project at ["
            <> fromString (haduiProjectRoot uio)
            <> "]"
        logDebug $ "hadui with-ghc: \n  " <> display (withGHC cfg)
        logDebug $ "hadui ghci-options: \n" <> display
            (T.unlines $ ("  " <>) <$> ghciOptions cfg)
        logDebug $ "hadui ghc-options: \n" <> display
            (T.unlines $ ("  " <>) <$> ghcOptions cfg)

    -- serve websockets in background threads
    let
        listenWS addr =
            bracket
                    (socket (addrFamily addr)
                            (addrSocketType addr)
                            (addrProtocol addr)
                    )
                    close
                $ \sock -> do
                    -- close-on-exec for this listening socket
                      withFdSocket sock setCloseOnExecIfNeeded

                    -- TODO on Ubuntu 18.04, it may fail even port
                    --      not really in use without reuse addr,
                    -- it'll create confusion when another process
                    -- is accepting the ws request, we'd prefer not
                    -- to proceed if we can not exclusively listen
                    -- the port.
                      setSocketOption sock ReuseAddr 1

                      bind sock $ addrAddress addr
                      listen sock 200

                      let
                          acceptWS = do
                              (conn, wsPeerAddr) <- accept sock
                              let
                                  serveWSC = do
                                      runUIO uio
                                          $  logDebug
                                          $  "hadui ws accepted: "
                                          <> display (tshow wsPeerAddr)
                                      pconn <-
                                          WS.makePendingConnection conn
                                              $ WS.defaultConnectionOptions
                                                    { WS.connectionStrictUnicode =
                                                        True
                                                    }
                                      wsc <- WS.acceptRequest pconn
                                      haduiServeWS wsc uio
                              _ <- forkFinally serveWSC $ \case
                                  Left exc ->
                                      runUIO uio
                                          $  logError
                                          $  "hadui failed with ws serving: "
                                          <> display (tshow exc)
                                  Right _ -> pure ()
                              acceptWS -- tail recursion
                      acceptWS
    liftIO $ do
        addrs <- getAddrInfo
            (Just $ defaultHints { addrSocketType = Stream })
            (Just $ T.unpack $ bindInterface cfg)
            (Just $ show $ wsPort cfg)
        for_ addrs $ \addr -> forkFinally (listenWS addr) $ \case
            Left exc ->
                runUIO uio
                    $  logError
                    $  "hadui failed with ws listening: "
                    <> display (tshow exc)
            Right _ -> pure ()



    -- continue to serve http in main thread
    let httpCfg =
            Snap.setBind (encodeUtf8 $ bindInterface cfg)
                $ Snap.setPort (httpPort cfg)
                $ Snap.setStartupHook httpListening
                $ Snap.setVerbose False
                $ Snap.setAccessLog Snap.ConfigNoLog
                $ Snap.setErrorLog (Snap.ConfigIoLog logHttpError) mempty
        logHttpError msgBytes = case decodeUtf8' msgBytes of
            Left  exc -> error $ "hadui unexpected encoding error: " ++ show exc
            Right msg -> runUIO uio $ logError $ display msg
        httpListening httpInfo = runUIO uio $ do
            listenAddrs <- liftIO
                $ sequence (getSocketName <$> Snap.getStartupSockets httpInfo)
            logInfo $ "hadui available at: " <> (display . T.unwords)
                (("http://" <>) . tshow <$> listenAddrs)

        dirServCfg   = Snap.fancyDirectoryConfig
        prjResRoot   = haduiProjectRoot uio </> "hadui"
        prjFrontFile = prjResRoot </> "front.html"
        !wsPortBytes = encodeUtf8 $ tshow $ wsPort cfg

    liftIO
        $   Snap.httpServe httpCfg
        -- serving ws port inquiry from js
        $   Snap.path ":" (Snap.writeBS wsPortBytes)
        -- map '/' to front.html in either directory
        <|> Snap.path
                ""
                (liftIO (D.doesFileExist prjFrontFile) >>= \case
                    True  -> Snap.serveFile prjFrontFile
                    False -> Snap.serveFile (haduiResRoot </> "front.html")
                )
        -- other static files
        <|> Snap.serveDirectoryWith dirServCfg prjResRoot
        <|> Snap.serveDirectoryWith dirServCfg haduiResRoot


websocketFromFD :: Int -> IO WS.Connection
websocketFromFD fd = do
    sock  <- mkSocket (fromIntegral fd)
    pconn <- WS.makePendingConnection sock
        $ WS.defaultConnectionOptions { WS.connectionStrictUnicode = True }
    WS.acceptRequest pconn

