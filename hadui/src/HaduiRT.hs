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
{-# LANGUAGE ParallelListComp #-}

-- | Hadui runtime
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
import           RIO.List
import qualified RIO.Text                      as T
import qualified RIO.Text.Lazy                 as TL
import           RIO.FilePath

import           System.Posix.Process

import qualified Data.ByteString.Builder       as SB

import qualified GHC
import qualified Debugger                      as GHC
import qualified HscTypes                      as GHC
import qualified ErrUtils                      as GHC
import qualified Panic                         as GHC
import qualified Outputable                    as GHC
import qualified GhcMonad                      as GHC

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
-- is open at each Hadui application's discrepancy.
--
-- A Hadui aware stack project implements websocket methods in 
-- `hadui/hadui-custom.js` under the root directory.
uiComm :: A.ToJSON a => a -> UIO ()
uiComm jsonCmd = do
    uio <- ask
    let gil         = haduiGIL uio
-- aeson will crash the process if 'txtPayload' is lazy here
        !txtPayload = A.encode jsonCmd
        txtPacket   = WS.Text txtPayload Nothing
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
        $ flip GHC.reflectGhc ghcSession
        $ GHC.handleGhcException logGhcExc
        $ GHC.handleSourceError logSrcError --
                                ghcAction

haduiExecStmt :: Text -> UIO ()
haduiExecStmt stmt = do
    uio <- ask
    haduiExecGhc $ do
        -- force the result to have it thoroughly executed
        !execResult <- GHC.execStmt
            ("mustUIO $\n" -- hint required type with this line prepended
                           ++ T.unpack stmt)
            -- have 'execLineNumber' start from 0 so line numbers in error
            -- report will match original source
            GHC.execOptions { GHC.execLineNumber = 0
                            , GHC.execSourceFile = "<hadui-adhoc>"

                    -- XXX RunAndLogSteps ~= :trace
                            , GHC.execSingleStep = GHC.RunAndLogSteps
                    -- TODO if disabling TRACE can save us enough run time,
                    -- show the option as a checkbox besides [CRUNCH!] btn.
                            }

        case execResult of
            GHC.ExecBreak brkNames _mbBrkInf -> do
                -- pretty print uncaught exception, and traced history
                dynFlags <- GHC.getInteractiveDynFlags
                errVars  <- catMaybes `liftM` mapM GHC.lookupName brkNames
                errTerms <- mapM (GHC.obtainTermFromId maxBound True)
                                 [ i | GHC.AnId i <- errVars ]
                errDocs <- mapM GHC.showTerm errTerms
                let
                    errDesc =
                        T.unwords
                            $   (T.pack . GHC.showSDocDumpOneLine dynFlags)
                            <$> errDocs
                resumes    <- GHC.getResumeContext
                errDetails <- case resumes of
                    [] -> return "Not stopped at a breakpoint, how strange."
                    (r : _) -> do
                        let hist = GHC.resumeHistory r
                            ppr =
                                T.pack
                                    . (GHC.showSDocDumpOneLine dynFlags)
                                    . GHC.ppr
                        if null hist
                            then
                                return
                                    "Empty history. `-fobject-code` enabled?\n\
                                    \Comment it out from `hadui.yaml`, run `stack purge`,\n\
                                    \then restart hadui."
                            else do
                                pans <- mapM GHC.getHistorySpan hist
                                let names = map GHC.historyEnclosingDecls hist
                                return $ T.unlines
                                    [ T.pack (unwords name)
                                      <> " ("
                                      <> ppr pan
                                      <> ")"
                                    | name <-  names | pan <- pans
                                    ]
                runUIO uio $ do
                    logWarn
                        $  display
                        $  errDesc
                        <> "\n===\n"
                        <> errDetails
                        <> "--- while exec stmt:\n"
                        <> stmt
                        <> "\n===uncaught error"
                    uiLog $ DetailedErrorMsg errDesc errDetails

                _ <- GHC.abandonAll -- todo consider resumption ?
                pure ()

            GHC.ExecComplete xResult _xAlloc -> case xResult of
                Left exc -> runUIO uio $ do
                    -- Hadui is set to break-on-error, this should not happen,
                    -- anyway log the unexpected error, just in case
                    let !errDetails = tshow exc
                    logWarn
                        $  display
                        $  "runtime error\n===\n"
                        <> errDetails
                        <> "\n--- while exec stmt:\n"
                        <> stmt
                        <> "\n===runtime error"
                    uiLog $ DetailedErrorMsg "runtime error" errDetails

                -- the normal case for vast majority
                Right _ -> pure ()


-- serve a ws until it's disconnected
haduiServeWS :: UserInterfaceOutput -> WS.Connection -> IO ()
haduiServeWS uio wsc = do
    runUIO uio $ withHaduiFront wsc $ uiLog $ DetailedMsg
        "Hadui ready for stack project at: "
        (T.pack $ haduiProjectRoot uio)
    let
        keepServingPkt = do
            pkt <- WS.receiveDataMessage wsc
            case pkt of
                (WS.Text _bytes (Just pktText)) ->
                    runUIO uio
                        $ withHaduiFront wsc
                        $ haduiExecStmt
                        $ TL.toStrict pktText
                (WS.Binary _bytes) -> do
                    runUIO uio
                        $ logError "Hadui received binary packet from ws ?!"
                    WS.sendCloseCode wsc 1003 ("?!?" :: Text)
                _ -> do
                    runUIO uio $ logError
                        "Hadui received unknown packet from ws ?!"
                    WS.sendCloseCode wsc 1003 ("?!?" :: Text)
            -- anyway we should continue receiving, even after close request sent, 
            -- we are expected to process a CloseRequest ctrl message from peer.
            keepServingPkt
    keepServingPkt `catch` \case
        WS.CloseRequest closeCode closeReason
            | closeCode == 1000 || closeCode == 1001 -> runUIO uio
            $ logDebug "Hadui ws closed normally."
            | otherwise -> do
                runUIO uio
                    $  logError
                    $  display
                    $  "Hadui ws closed with code "
                    <> display closeCode
                    <> " and reason ["
                    <> (Utf8Builder $ SB.lazyByteString closeReason)
                    <> "]"
                -- yet still try to receive ctrl msg back from peer
                haduiServeWS uio wsc
        WS.ConnectionClosed -> runUIO uio $ logDebug "Hadui ws disconnected."
        _                   -> runUIO uio $ logError "Hadui unexpected ws error"


haduiPubServer :: GHC.Ghc ()
haduiPubServer = do
    dataDir <- liftIO Meta.getDataDir
    let haduiResRoot = dataDir </> "web"
    unlessM (liftIO $ D.doesDirectoryExist haduiResRoot)
        $ error "Hadui web resource directory missing ?!"

    uio <- initUIO

    let cfg = haduiConfig uio
    runUIO uio $ do
        logDebug
            $  "Hadui using resource dir: ["
            <> fromString haduiResRoot
            <> "]"
        logInfo
            $  "Hadui publishing project at ["
            <> fromString (haduiProjectRoot uio)
            <> "]"

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
                    -- this works with network-2.0
                      setCloseOnExecIfNeeded $ fdSocket listeningSock
                    -- following works with network-3.x
                    --  withFdSocket listeningSock setCloseOnExecIfNeeded

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
                                  $  "Hadui ws accepted: "
                                  <> display (tshow wsPeerAddr)
                              _ <- forkFinally (wscHandler conn) $ \wsResult ->
                                  do
                                      case wsResult of
                                          Left exc ->
                                              runRIO env
                                                  $ logError
                                                  $ "Hadui failed with ws handling: "
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
            Left exc -> do
                runRIO env
                    $  logError
                    $  "Hadui failed with ws listening: "
                    <> display (tshow exc)
                exitImmediately $ ExitFailure 5
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
            Left  exc -> error $ "Hadui unexpected encoding error: " ++ show exc
            Right msg -> runRIO env $ logError $ display msg
        httpListening httpInfo = do
            listenAddrs <- sequence
                (getSocketName <$> Snap.getStartupSockets httpInfo)
            runRIO env
                $  logInfo
                $  "Hadui available at: "
                <> (display . T.unwords)
                       (("http://" <>) . tshow <$> listenAddrs)

        prjResRoot    = prjRoot </> "hadui"
        prjFrontFile  = prjResRoot </> "front.html"
        ootbFrontFile = resRoot </> "front.html"
        !wsPortBytes  = encodeUtf8 $ tshow $ wsPort cfg
        dirServCfg    = Snap.fancyDirectoryConfig

    liftIO $ do
        -- todo log the overlay dirs for debug purpose ?
        overlayResRoots <- resolveHaduiResRoots $ overlayPackages cfg

        Snap.httpServe httpCfg
            -- serving ws port inquiry from js
            $   Snap.path ":" (Snap.writeBS wsPortBytes)

            -- map '/' to front.html from either the project of matter, or
            -- hadui package OOTB
            -- note front.html from any overlay package is not mapped to '/'
            <|> Snap.path
                    "" -- this is tested on each request ad-hoc, so no
                    -- restart needed after the project add/rm the page
                    (liftIO (D.doesFileExist prjFrontFile) >>= \case
                        True  -> Snap.serveFile prjFrontFile
                        False -> Snap.serveFile ootbFrontFile
                    )

            <|> (foldl
                    (<|>)
            -- resource files from the project of matter
                    (Snap.serveDirectoryWith dirServCfg prjResRoot)

            -- resource files from packages listed in the overlay cfg
                    ((Snap.serveDirectoryWith dirServCfg) <$> overlayResRoots)
                )

            -- fallback lastly to OOTB resource root from the hadui package
            <|> Snap.serveDirectoryWith dirServCfg resRoot


haduiDevServer :: Int -> GHC.Ghc ()
haduiDevServer wsfd = do
    uio <- initUIO
    let wsfd' = fromIntegral wsfd

    liftIO $ do
        setNonBlockIfNeeded wsfd'
        -- XXX it's not trivial to decied the fd is AF_INET or AF_INET6 here,
        -- but we wont't have to with network-3.x, mkSocket will take only fd.
        -- as far as lts stays with network-2.8, we blindly assume it's IPv4,
        -- won't fix this if anyone suffers using IPv6 for Hadui.
        sock  <- mkSocket wsfd' AF_INET Stream defaultProtocol Connected
        pconn <- WS.makePendingConnection sock $ WS.defaultConnectionOptions
            { WS.connectionStrictUnicode = True
            }
        wsc <- WS.acceptRequest pconn
        runUIO uio
            $  logDebug
            $  "Hadui development mode serving wsfd: "
            <> display (tshow wsfd)
        haduiServeWS uio wsc

