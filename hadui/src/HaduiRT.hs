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
{-# LANGUAGE LambdaCase #-}

-- | hadui runtime
module HaduiRT
    ( HaduiConfig(..)
    , loadConfig
    , backendLogOptions
    , HaduiFirstProcess(..)
    , runHadUI
    )
where

import           RIO                     hiding ( unlines
                                                , unwords
                                                )
import           RIO.Text
import           RIO.Process                    ( HasProcessContext(..)
                                                , ProcessContext
                                                )
import           UnliftIO.Concurrent
import           UnliftIO.Process
import           RIO.FilePath

import qualified System.Directory              as D

import           Data.Yaml.Aeson

import           System.Posix.IO                ( closeFd )
import           System.Posix.Types

import           Network.Socket

import           Snap.Core               hiding ( logError )
import           Snap.Http.Server
import           Snap.Util.FileServe


loadConfig :: IO (FilePath, HaduiConfig)
loadConfig =
    readProcessWithExitCode "/usr/bin/env"
                            ["stack", "path", "--project-root"]
                            []
        >>= \case
                (ExitSuccess, outBytes, "") ->
                    let stackPrjRoot  = (unpack . strip . pack) outBytes
                        haduiYamlFile = stackPrjRoot </> "hadui.yaml"
                    in  D.doesFileExist haduiYamlFile
                            >>= \case
                                    True  -> decodeFileThrow haduiYamlFile
                                    -- parse empty dict for all defaults
                                    False -> decodeThrow "{}"
                            >>= \cfg -> return (stackPrjRoot, cfg)
                _ -> error "Can not determine stack project root."

data HaduiConfig = HaduiConfig {
    bindInterface :: Text
    , httpPort :: Int
    , wsPort :: Int
    , logLevel :: Text
    , withGHC :: Text
    , ghciOptions :: [Text]
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
            .!= "ghc"
            <*> v
            .:? "ghci-options"
            .!= []
    parseJSON _ = fail "Expected Object for Config value"


backendLogOptions :: HaduiConfig -> IO LogOptions
backendLogOptions cfg = do
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
    , appProcessContext :: !ProcessContext
    }

instance HasLogFunc HaduiFirstProcess where
    logFuncL = lens appLogFunc (\x y -> x { appLogFunc = y })
instance HasProcessContext HaduiFirstProcess where
    processContextL =
        lens appProcessContext (\x y -> x { appProcessContext = y })


runHadUI :: RIO HaduiFirstProcess ()
runHadUI = do
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
    logDebug $ "hadui ghci-options: \n" <> display
        (unlines $ ("  " <>) <$> ghciOptions cfg)


    addrs <- liftIO $ getAddrInfo
        (Just $ defaultHints { addrSocketType = Stream })
        (Just $ unpack $ bindInterface cfg)
        (Just $ show $ wsPort cfg)
    let listenWS addr = do
            sock <- socket (addrFamily addr)
                           (addrSocketType addr)
                           (addrProtocol addr)
            setSocketOption sock ReuseAddr 0
            withFdSocket sock setCloseOnExecIfNeeded
            bind sock $ addrAddress addr
            listen sock 20
            return sock
        wsFailure :: SomeException -> RIO HaduiFirstProcess ()
        wsFailure exc =
            logError $ "hadui failed with ws - " <> display (tshow exc)

    -- serve websockets in background threads
    for_ addrs $ \addr -> do
        _ <- forkIO $ handle wsFailure $ bracket (liftIO $ listenWS addr)
                                                 (liftIO . close)
                                                 servWS
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
            logInfo $ "hadui available at: " <> (display . unwords)
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


servWS :: Socket -> RIO HaduiFirstProcess ()
servWS sock = do
    app  <- ask

    addr <- liftIO $ getSocketName sock
    logInfo $ "hadui listening ws://" <> display (tshow addr)

    let cfg            = haduiConfig app
        !ghcExecutable = show $ withGHC cfg
        !extraOpts =
            RIO.concat [ ["--ghci-options", show opt] | opt <- ghciOptions cfg ]

        acceptWSC = do
            (conn, wsPeerAddr) <- liftIO $ accept sock
            logDebug $ "hadui ws accepted: " <> display (tshow wsPeerAddr)
            return conn
        closeHandle conn = liftIO $ do
            wsfd <- unsafeFdSocket conn
            closeFd $ Fd wsfd

        spawnHandler conn = do
            wsfd <- liftIO $ unsafeFdSocket conn

            _    <- createProcess $ (proc
                                        "/usr/bin/env"
                                        (  [ "env"
                                           , "stack"
                                           , "ghci"
                                           , "--with-ghc"
                                           , ghcExecutable
                                           , "--ghci-options"
                                           , "-e ':frontend HaduiGHCi'"
                                           , "--ghci-options"
                                           , "-ffrontend-opt " ++ show wsfd
                                           ]
                                        ++ extraOpts -- opts from hadui.yaml
                                        )
                                    )
                { std_in  = NoStream
                , std_out = Inherit
                , std_err = Inherit
                }

            logDebug $ "hadui ws handler started for fd " <> display
                (tshow wsfd)

    -- always close fd of the ws socket in parent process
    forever $ bracket acceptWSC closeHandle spawnHandler
