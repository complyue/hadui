{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE BangPatterns #-}

module HaduiDev
    ( frontendPlugin
    )
where

import           RIO
import qualified RIO.Text                      as T
import qualified RIO.Text.Lazy                 as TL
import qualified Data.Text.Lazy.Encoding       as TLE

import           System.Posix.Types
import           System.Posix.Process
import           System.Posix.IO

import           UnliftIO.Concurrent

import           Snap.Core               hiding ( logError )
import           Snap.Http.Server
import           Snap.Util.FileServe

import qualified GHC
import qualified HscTypes                      as GHC
import qualified ErrUtils                      as GHC
import qualified Panic                         as GHC
import qualified GhcMonad                      as GHC
import qualified GhcPlugins                    as GHC

import qualified Network.WebSockets            as WS

import           UIO
import           HaduiRT


frontendPlugin :: GHC.FrontendPlugin
frontendPlugin = GHC.defaultFrontendPlugin { GHC.frontend = gfeMain }  where
    -- GHC frontend plugin entry point
    gfeMain flags args = do
        case args of
            [] -> pure ()
            _  -> GHC.throwGhcException
                $ GHC.CmdLineError "WebUI expects NO src file"
        case flags of
            [wsfdStr] -> case (readMaybe wsfdStr :: Maybe Int) of
                Nothing ->
                    GHC.throwGhcException $ GHC.CmdLineError "Invalid wsfd"
                Just wsfd -> (liftIO $ initUIO wsfd) >>= interactiveUI
            _ -> GHC.throwGhcException
                $ GHC.CmdLineError "WebUI expects single wsfd"


interactiveUI :: UserInterfaceOutput -> GHC.Ghc ()
interactiveUI uio = do
    runUIO uio $ uiLog $ DetailedMsg "hadui serving project at: "
                                     (T.pack $ haduiProjectRoot uio)

    -- obtain GHC session for the interaction course
    ghcSession <- GHC.reifyGhc return

    -- to have GHC errors caught and displayed in web UI
    let logGhcExc exc = do
            let errDetails = tshow exc
            runUIO uio $ uiLog $ DetailedErrorMsg "GHC error: " errDetails
        logSrcError err = do
            dynFlags <- GHC.getSessionDynFlags
            let errDetails = T.pack $ unlines $ map
                    (GHC.showSDoc dynFlags)
                    (GHC.pprErrMsgBagWithLoc $ GHC.srcErrorMessages err)
            runUIO uio $ uiLog $ DetailedErrorMsg "Source error: " errDetails
        execGhc :: GHC.Ghc () -> GHC.Ghc ()
        execGhc action =
            GHC.handleGhcException logGhcExc
                $ GHC.handleSourceError logSrcError action
        execStmt :: Text -> GHC.Ghc ()
        execStmt stmt = execGhc $ do
            _execResult <- GHC.execStmt
                ("mustUIO $\n" ++ T.unpack stmt)
                GHC.execOptions { GHC.execSourceFile = "<hadui-adhoc>"
                                , GHC.execLineNumber = 0
                                }
            -- TODO say sth about the result to web UI ?
            return ()

    execGhc $ do
        -- make sure 'UIO' is in scope
        GHC.getContext
            >>= GHC.setContext
            .   ((GHC.IIDecl $ GHC.simpleImportDecl $ GHC.mkModuleName "UIO") :)
        _ <- GHC.runDecls "default (Text,Int)"
        -- to allow literal Text/Int without explicit type anno
        return ()

    -- every time a text packet received, just execute it as Haskell statement
    let !wsc = haduiWebSocket uio
        interpretTextPacket :: TL.Text -> GHC.Ghc ()
        interpretTextPacket pkt = let stmt = TL.toStrict pkt in execStmt stmt

    -- serving ws forever
    let servWS :: IO ()
        servWS = do
            pkt <- WS.receiveDataMessage wsc
            case pkt of
                (WS.Text _bytes (Just pktText)) ->
                    GHC.reflectGhc (interpretTextPacket pktText) ghcSession
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
            servWS
    liftIO $ catch servWS $ \exc -> do
        case exc of
            WS.CloseRequest closeCode closeReason
                | closeCode == 1000 || closeCode == 1001
                -> runUIO uio $ logDebug "hadui ws closed normally."
                | otherwise
                -> runUIO uio
                    $  logError
                    $  "hadui ws closed with code "
                    <> display closeCode
                    <> " and reason ["
                    <> display (TLE.decodeUtf8 closeReason)
                    <> "]"
            WS.ConnectionClosed ->
                runUIO uio $ logDebug $ "hadui ws disconnected forcefully."
            _ -> do
                runUIO uio $ logError "hadui unexpected ws error"
                WS.sendCloseCode wsc 1003 ("?!?" :: Text)
        -- yet still try to receive ctrl msg back from peer
        servWS


data HaduiDevServer = HaduiDevServer {
    stackProjectRoot :: FilePath
    , haduiResourceRoot :: FilePath
    , haduiConfig :: HaduiConfig
    , appLogFunc :: !LogFunc
    }

instance HasLogFunc HaduiDevServer where
    logFuncL = lens appLogFunc (\x y -> x { appLogFunc = y })


haduiDevRun :: RIO HaduiDevServer ()
haduiDevRun = do
    app <- ask
    let cfg = haduiConfig app

    logInfo
        $  "hadui serving project at ["
        <> fromString (stackProjectRoot app)
        <> "]"
    logDebug
        $  "hadui using resource dir: ["
        <> fromString (haduiResourceRoot app)
        <> "]"
    logDebug $ "hadui with-ghc: \n  " <> display (withGHC cfg)
    logDebug $ "hadui ghci-options: \n" <> display
        (T.unlines $ ("  " <>) <$> ghciOptions cfg)
    logDebug $ "hadui ghc-options: \n" <> display
        (T.unlines $ ("  " <>) <$> ghcOptions cfg)


    addrs <- liftIO $ getAddrInfo
        (Just $ defaultHints { addrSocketType = Stream })
        (Just $ T.unpack $ bindInterface cfg)
        (Just $ show $ wsPort cfg)
    let listenWS addr = do
            sock <- socket (addrFamily addr)
                           (addrSocketType addr)
                           (addrProtocol addr)

            -- TODO on Ubuntu 18.04, it may fail even port
            --      not really in use without reuse addr,
            -- it'll create confusion when another process
            -- is accepting the ws request, we'd prefer not
            -- to proceed if we can not exclusively listen
            -- the port.
            setSocketOption sock ReuseAddr 1

            withFdSocket sock setCloseOnExecIfNeeded
            bind sock $ addrAddress addr
            listen sock 20
            return sock
        wsFailure :: SomeException -> RIO HaduiDevServer ()
        wsFailure exc = do
            logError $ "hadui failed with ws serving: " <> display (tshow exc)
            liftIO $ exitImmediately $ ExitFailure 1

    -- serve websockets in background threads
    for_ addrs $ \addr -> do
        _ <- forkIO $ handle wsFailure $ bracket (liftIO $ listenWS addr)
                                                 (liftIO . close)
                                                 upstartHandler
        return ()
    -- continue to serve http in main thread

    let httpCfg =
            setBind (encodeUtf8 $ bindInterface cfg)
                $ setPort (httpPort cfg)
                $ setStartupHook httpListening
                $ setVerbose False
                $ setAccessLog ConfigNoLog
                $ setErrorLog (ConfigIoLog logHttpError) mempty
        logHttpError msgBytes = case decodeUtf8' msgBytes of
            Left  exc -> error $ "hadui unexpected encoding error: " ++ show exc
            Right msg -> runRIO app $ logError $ display msg
        httpListening httpInfo = runRIO app $ do
            listenAddrs <- liftIO
                $ sequence (getSocketName <$> getStartupSockets httpInfo)
            logInfo $ "hadui available at: " <> (display . T.unwords)
                (("http://" <>) . tshow <$> listenAddrs)

        dirServCfg   = fancyDirectoryConfig
        prjResRoot   = stackProjectRoot app </> "hadui"
        haduiResRoot = haduiResourceRoot app
        prjFrontFile = prjResRoot </> "front.html"
        !wsPortBytes = encodeUtf8 $ tshow $ wsPort cfg

    liftIO
        $   httpServe httpCfg
        -- serving ws port inquiry from js
        $   path ":" (writeBS wsPortBytes)
        -- map '/' to front.html in either directory
        <|> path
                ""
                (liftIO (D.doesFileExist prjFrontFile) >>= \case
                    True  -> serveFile prjFrontFile
                    False -> serveFile (haduiResRoot </> "front.html")
                )
        -- other static files
        <|> serveDirectoryWith dirServCfg prjResRoot
        <|> serveDirectoryWith dirServCfg haduiResRoot


upstartHandler :: Socket -> RIO HaduiDevServer ()
upstartHandler sock = do
    app  <- ask

    addr <- liftIO $ getSocketName sock
    logInfo $ "hadui listening ws://" <> display (tshow addr)

    let cfg            = haduiConfig app
        !ghcExecutable = T.unpack $ withGHC cfg
        !extraOpts     = RIO.concat
            (  [ ["--ghci-options", show opt] | opt <- ghciOptions cfg ]
            ++ [ ["--ghc-options", show opt] | opt <- ghcOptions cfg ]
            )

        acceptWSC = liftIO $ accept sock
        closeHandle (conn, _) = liftIO $ do
            wsfd <- unsafeFdSocket conn
            closeFd $ Fd wsfd

        spawnHandler (conn, wsPeerAddr) = do
            logDebug $ "hadui ws accepted: " <> display (tshow wsPeerAddr)
            liftIO $ do
                wsfd <- unsafeFdSocket conn

                -- clear FD_CLOEXEC flag so it can be passed to subprocess
                setFdOption (Fd wsfd) CloseOnExec False

                -- launch `stack ghci` with hadui's GHC frontend to serve the ws
                pid <- forkProcess $ executeFile
                    "/usr/bin/env"
                    False
                    ([ "stack"
                     , "ghci"
                     , "--with-ghc"
                     , ghcExecutable

                     -- use UIO which reexports RIO as prelude
                     , "--ghc-options"
                     , "-XNoImplicitPrelude"

                     -- really hope that Haskell the language unify the string
                     -- types (with utf8 seems the norm) sooner than later
                     , "--ghc-options"
                     , "-XOverloadedStrings"

                     -- to allow literal Text/Int without explicit type anno
                     , "--ghc-options"
                     , "-XExtendedDefaultRules"

                     -- the frontend trigger
                     , "--ghci-options"
                     , "-e \":frontend HaduiGHCi\" -ffrontend-opt " ++ show wsfd
                     ]
                    ++ extraOpts -- opts from hadui.yaml
                    )
                    Nothing

                runRIO app
                    $  logDebug
                    $  display
                    $  "hadui started ws handler pid: "
                    <> tshow pid

                -- say sth on exit of the subprocess, this also prevents
                -- the exited subprocess becoming a zombie.
                void $ forkIO $ do
                    ps <- getProcessStatus True True pid
                    void $ runRIO app $ case ps of
                        Nothing -> error "the impossible happens here"
                        Just (Exited ExitSuccess) ->
                            logDebug
                                $  display
                                $  "hadui ws handler process "
                                <> tshow pid
                                <> " exited."
                        Just (Exited exitCode) ->
                            logError
                                $  display
                                $  "hadui ws handler process "
                                <> tshow pid
                                <> " exited with "
                                <> tshow exitCode
                        Just (Terminated sig coreDumped) ->
                            logWarn
                                $  display
                                $  "hadui ws handler process "
                                <> tshow pid
                                <> " killed by signal "
                                <> tshow sig
                                <> if coreDumped
                                       then " with core dumped"
                                       else ""
                        Just (Stopped sig) ->
                            logWarn
                                $  display
                                $  "hadui ws handler process "
                                <> tshow pid
                                <> " stopped by signal "
                                <> tshow sig

    -- always close fd of the ws socket in parent process
    forever $ bracket acceptWSC closeHandle spawnHandler
