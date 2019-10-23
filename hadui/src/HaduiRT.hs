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

    -- common routines among pub/dev/debug modes
    , haduiPubServer
    , haduiDevServer
    , haduiListenWSC
    , haduiServeHttp
    )
where

import           RIO
import qualified RIO.Text                      as T
import qualified RIO.Text.Lazy                 as TL
import           RIO.FilePath

import qualified Data.ByteString.Builder       as SB

import qualified GHC
import qualified HscTypes                      as GHC
import qualified ErrUtils                      as GHC
import qualified Panic                         as GHC
import qualified GhcMonad                      as GHC
import qualified GhcPlugins                    as GHC

import qualified System.Directory              as D
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
    let gil        = haduiGIL uio
        txtPayload = A.encode jsonCmd
        txtPacket  = WS.Text txtPayload Nothing
    liftIO $ do
        noWSC <- isEmptyMVar gil
        if noWSC
            then
                runUIO uio
                $  logError
                $  display
                $  "No ws in context for comm of: "
                <> (Utf8Builder $ SB.lazyByteString txtPayload)
            else do
                wsc <- readMVar gil
                WS.sendDataMessage wsc txtPacket
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
haduiServeWS :: UserInterfaceOutput -> WS.Connection -> IO ()
haduiServeWS uio wsc = do
    runUIO uio $ withHaduiFront wsc $ uiLog $ DetailedMsg
        "hadui ready for stack project at: "
        (T.pack $ haduiProjectRoot uio)
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
    catch servOnePkt $ \case
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
                haduiServeWS uio wsc
        WS.ConnectionClosed -> runUIO uio $ logDebug "hadui ws disconnected."
        _                   -> runUIO uio $ logError "hadui unexpected ws error"


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
    runRIO uio $ haduiListenWSC cfg close $ \conn -> do
        pconn <- WS.makePendingConnection conn
            $ WS.defaultConnectionOptions { WS.connectionStrictUnicode = True }
        wsc <- WS.acceptRequest pconn
        haduiServeWS uio wsc

    -- continue to serve http in main thread
    runRIO uio $ haduiServeHttp cfg (haduiProjectRoot uio) haduiResRoot


haduiListenWSC
    :: HasLogFunc env
    => HaduiConfig
    -> (Socket -> IO ())
    -> (Socket -> IO ())
    -> RIO env ()
haduiListenWSC cfg wscDisposer wscHandler = do
    env <- ask

    let
        listenWS addr =
            bracket
                    (socket (addrFamily addr)
                            (addrSocketType addr)
                            (addrProtocol addr)
                    )
                    close
                $ \listeningSock -> do
                    -- close-on-exec for this listening socket
                      withFdSocket listeningSock setCloseOnExecIfNeeded

                    -- TODO on Ubuntu 18.04, it may fail even port
                    --      not really in use without reuse addr,
                    -- it'll create confusion when another process
                    -- is accepting the ws request, we'd prefer not
                    -- to proceed if we can not exclusively listen
                    -- the port.
                      setSocketOption listeningSock ReuseAddr 1

                      bind listeningSock $ addrAddress addr
                      listen listeningSock 200

                      let
                          acceptWSC = do
                              (conn, wsPeerAddr) <- accept listeningSock
                              runRIO env
                                  $  logDebug
                                  $  "hadui ws accepted: "
                                  <> display (tshow wsPeerAddr)
                              _ <- forkFinally (wscHandler conn) $ \wsResult ->
                                  do
                                      case wsResult of
                                          Left exc ->
                                              runRIO env
                                                  $ logError
                                                  $ "hadui failed with ws handling: "
                                                  <> display (tshow exc)
                                          Right _ -> pure ()
                                      wscDisposer conn
                              acceptWSC -- tail recursion
                      acceptWSC
    liftIO $ do
        addrs <- getAddrInfo
            (Just $ defaultHints { addrSocketType = Stream })
            (Just $ T.unpack $ bindInterface cfg)
            (Just $ show $ wsPort cfg)
        for_ addrs $ \addr -> forkFinally (listenWS addr) $ \case
            Left exc ->
                runRIO env
                    $  logError
                    $  "hadui failed with ws listening: "
                    <> display (tshow exc)
            Right _ -> pure ()

haduiServeHttp
    :: HasLogFunc env => HaduiConfig -> FilePath -> FilePath -> RIO env ()
haduiServeHttp cfg prjRoot resRoot = do
    env <- ask

    let httpCfg =
            Snap.setBind (encodeUtf8 $ bindInterface cfg)
                $ Snap.setPort (httpPort cfg)
                $ Snap.setStartupHook httpListening
                $ Snap.setVerbose False
                $ Snap.setAccessLog Snap.ConfigNoLog
                $ Snap.setErrorLog (Snap.ConfigIoLog logHttpError) mempty
        logHttpError msgBytes = case decodeUtf8' msgBytes of
            Left  exc -> error $ "hadui unexpected encoding error: " ++ show exc
            Right msg -> runRIO env $ logError $ display msg
        httpListening httpInfo = do
            listenAddrs <- sequence
                (getSocketName <$> Snap.getStartupSockets httpInfo)
            runRIO env
                $  logInfo
                $  "hadui available at: "
                <> (display . T.unwords)
                       (("http://" <>) . tshow <$> listenAddrs)

        dirServCfg   = Snap.fancyDirectoryConfig
        prjResRoot   = prjRoot </> "hadui"
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
                    False -> Snap.serveFile (resRoot </> "front.html")
                )
        -- other static files
        <|> Snap.serveDirectoryWith dirServCfg prjResRoot
        <|> Snap.serveDirectoryWith dirServCfg resRoot


haduiDevServer :: Int -> GHC.Ghc ()
haduiDevServer wsfd = do
    uio <- initUIO

    liftIO $ do
        sock  <- mkSocket (fromIntegral wsfd)
        pconn <- WS.makePendingConnection sock
            $ WS.defaultConnectionOptions { WS.connectionStrictUnicode = True }
        wsc <- WS.acceptRequest pconn
        runUIO uio
            $  logDebug
            $  "hadui development mode serving wsfd: "
            <> display (tshow wsfd)
        haduiServeWS uio wsc

