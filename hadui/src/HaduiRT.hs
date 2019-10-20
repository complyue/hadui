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

-- | hadui runtime
module HaduiRT
    ( UIO(..)
    , UserInterfaceOutput(..)
    , runUIO
    , mustUIO
    , initUIO
    , HaduiConfig(..)
    , loadHaduiConfig
    , haduiBackendLogOpts
    , HaduiFirstProcess(..)
    , runHadui
    )
where

import           RIO
import qualified RIO.Text                      as T
import           RIO.FilePath

import           System.IO.Unsafe

import           System.Posix.Types
import           System.Posix.Process
import           System.Posix.IO
import qualified System.Directory              as D

import           UnliftIO.Concurrent
import           UnliftIO.Process

import           Data.Yaml.Aeson

import           Network.Socket
import qualified Network.WebSockets            as WS

import           Snap.Core               hiding ( logError )
import           Snap.Http.Server
import           Snap.Util.FileServe


-- | The monad for User Interface Output
-- UIO is output only, conversely to IO (which stands for Input/Output),
-- user inputs shall be facilitated with a registry of 'MVar's,
-- those get filled with 'IoC' from UI widgets.
newtype UIO a = UIO { unUIO :: ReaderT UserInterfaceOutput IO a }
    deriving (Functor, Applicative, Monad, MonadIO,
        MonadReader UserInterfaceOutput, MonadThrow)

data UserInterfaceOutput = UserInterfaceOutput {
    haduiProjectRoot :: FilePath
    , haduiCommMutex :: MVar ()
    , haduiWebSocket :: WS.Connection

    , backendLogFunc :: !LogFunc
    }

instance HasLogFunc UserInterfaceOutput where
    logFuncL = lens backendLogFunc (\x y -> x { backendLogFunc = y })

runUIO :: MonadIO m => UserInterfaceOutput -> UIO a -> m a
runUIO uio (UIO (ReaderT f)) = liftIO (f uio)


initUIO :: Int -> IO UserInterfaceOutput
initUIO wsfd = do
    uninitialized <- isEmptyMVar _globalUIO
    unless uninitialized $ error "initUIO reentrance ?!"

    (stackPrjRoot, cfg) <- loadHaduiConfig
    lo                  <- haduiBackendLogOpts cfg
    -- todo teardown 'lf' once the log target needs,
    -- not needed as we only log to stderr by far
    (lf, _ :: IO ())    <- newLogFunc lo
    mu                  <- newMVar ()
    wsc                 <- websocketFromFD wsfd
    let uio = UserInterfaceOutput { haduiProjectRoot = stackPrjRoot
                                  , haduiCommMutex   = mu
                                  , haduiWebSocket   = wsc
                                  , backendLogFunc   = lf
                                  }
    putMVar _globalUIO uio
    runUIO uio
        $  logInfo
        $  "hadui interactive frontend serving wsfd="
        <> (display $ tshow wsfd)
    return uio

_globalUIO :: MVar UserInterfaceOutput
{-# NOINLINE _globalUIO #-}
_globalUIO = unsafePerformIO newEmptyMVar

-- | every statement will be lifted by this function into IO monad for GHCi to execute.
-- this also very explicitly hints the expected monad type for all statements to eval.
mustUIO :: NFData a => UIO a -> IO ()
mustUIO m = do
    uio <- readMVar _globalUIO
    v   <- runUIO uio m
    _   <- pure $!! v -- force it to be executed
    pure ()

websocketFromFD :: Int -> IO WS.Connection
websocketFromFD fd = do
    sock  <- mkSocket (fromIntegral fd)
    pconn <- WS.makePendingConnection sock
        $ WS.defaultConnectionOptions { WS.connectionStrictUnicode = True }
    WS.acceptRequest pconn



loadHaduiConfig :: IO (FilePath, HaduiConfig)
loadHaduiConfig =
    readProcessWithExitCode "/usr/bin/env"
                            ["stack", "path", "--project-root"]
                            ""
        >>= \case
                (ExitSuccess, outBytes, "") ->
                    let stackPrjRoot  = (T.unpack . T.strip . T.pack) outBytes
                        haduiYamlFile = stackPrjRoot </> "hadui.yaml"
                    in  D.doesFileExist haduiYamlFile
                            >>= \case
                                    -- TODO this segfaults on OSX in ws subprocess,
                                    --      to figure out the situation.
                                    True -> decodeFileThrow haduiYamlFile
                                    -- parse empty dict for all defaults
                                    _    -> decodeThrow "{}"
                            >>= \cfg -> return (stackPrjRoot, cfg)
                _ -> error "Can not determine stack project root."


data HaduiConfig = HaduiConfig {
    bindInterface :: Text
    , httpPort :: Int
    , wsPort :: Int
    , logLevel :: Text
    , withGHC :: Text
    , ghciOptions :: [Text]
    , ghcOptions :: [Text]
    } deriving (Eq,Show )

instance FromJSON HaduiConfig where
    parseJSON (Object v) =
        HaduiConfig
            <$> v
            .:? "bind-interface"
            .!= "127.0.0.1"
            <*> v
            .:? "http-port"
            .!= 5050
            <*> v
            .:? "ws-port"
            .!= 5051
            <*> v
            .:? "log-level"
            .!= "INFO"
            <*> v
            .:? "with-ghc"
            .!= "ghc-ife" -- TODO default to ghc once ':frontend' cmd is supported officially
            <*> v
            .:? "ghci-options"
            .!= ["-fobject-code"]
            <*> v
            .:? "ghc-options"
            .!= []
    parseJSON _ = fail "Expected Object for Config value"


haduiBackendLogOpts :: HaduiConfig -> IO LogOptions
haduiBackendLogOpts cfg = do
    let ll = case logLevel cfg of
            "DEBUG" -> LevelDebug
            "WARN"  -> LevelWarn
            "ERROR" -> LevelError
            _       -> LevelInfo
        verbose = ll < LevelWarn -- go verbose since info level
    lo <- logOptionsHandle stderr verbose
    return $ setLogMinLevel ll lo


data HaduiFirstProcess = HaduiFirstProcess {
    stackProjectRoot :: FilePath
    , haduiResourceRoot :: FilePath
    , haduiConfig :: HaduiConfig
    , appLogFunc :: !LogFunc
    }

instance HasLogFunc HaduiFirstProcess where
    logFuncL = lens appLogFunc (\x y -> x { appLogFunc = y })


runHadui :: RIO HaduiFirstProcess ()
runHadui = do
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
        wsFailure :: SomeException -> RIO HaduiFirstProcess ()
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


upstartHandler :: Socket -> RIO HaduiFirstProcess ()
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
