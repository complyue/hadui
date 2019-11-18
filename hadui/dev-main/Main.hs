{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE BangPatterns #-}

module Main
    ( main
    )
where

import           RIO
import           RIO.FilePath


import qualified System.Directory              as D
import           UnliftIO.Concurrent

import           Network.Socket

import           System.Posix.Types
import           System.Posix.Process
import           System.Posix.IO

import           HaduiCfg
import           HaduiRT

import qualified Paths_hadui                   as Meta


main :: IO ()
main = do
    prj     <- loadHaduiConfig

    dataDir <- Meta.getDataDir
    let haduiResRoot = dataDir </> "web"
    D.doesDirectoryExist haduiResRoot >>= \case
        True -> return ()
        _    -> error "Hadui web resource directory missing ?!"

    let !cfg = haduiCfg prj
    lo <- haduiBackendLogOpts cfg
    withLogFunc lo $ \lf ->
        let devApp = HaduiDevServer { haduiProjectRoot = projectRoot prj
                                    , haduiPrj         = prj
                                    , haduiDevLogFunc  = lf
                                    }
        in  runRIO devApp $ runDevServer haduiResRoot

data HaduiDevServer = HaduiDevServer {
    haduiProjectRoot :: FilePath
    , haduiPrj :: !HaduiProject
    , haduiDevLogFunc :: !LogFunc
    }

instance HasLogFunc HaduiDevServer where
    logFuncL = lens haduiDevLogFunc (\x y -> x { haduiDevLogFunc = y })


runDevServer :: FilePath -> RIO HaduiDevServer ()
runDevServer haduiResRoot = do
    devs <- ask
    let !prj = haduiPrj devs
        !cfg = haduiCfg prj

    logDebug $ "Hadui using resource dir: [" <> fromString haduiResRoot <> "]"
    logInfo
        $  "Hadui developing project at ["
        <> fromString (haduiProjectRoot devs)
        <> "]"

    let
        upstartHandler conn = do
            -- this works with network-2.x
            let wsfd = fdSocket conn
            -- following works with network-3.x
            -- wsfd <- unsafeFdSocket conn

            -- clear FD_CLOEXEC flag so it can be passed to subprocess
            setFdOption (Fd wsfd) CloseOnExec False

            -- launch `stack ghci` with hadui's GHC frontend to serve the ws
            pid <- forkProcess $ executeFile
                "/usr/bin/env"
                False
                (haduiGHCiCmdl prj "HaduiDev" [show wsfd])
                Nothing

            runRIO devs
                $  logDebug
                $  display
                $  "Hadui started dev process pid: "
                <> tshow pid

            -- say sth on exit of the subprocess, this also prevents
            -- the exited subprocess becoming a zombie.
            void $ forkIO $ do
                ps <- getProcessStatus True True pid
                void $ runRIO devs $ case ps of
                    Nothing -> error "the impossible happens here"
                    Just (Exited ExitSuccess) ->
                        logDebug
                            $  display
                            $  "Hadui dev process "
                            <> tshow pid
                            <> " exited."
                    Just (Exited exitCode) ->
                        logError
                            $  display
                            $  "Hadui dev process "
                            <> tshow pid
                            <> " exited with "
                            <> tshow exitCode
                    Just (Terminated sig coreDumped) ->
                        logWarn
                            $  display
                            $  "Hadui dev process "
                            <> tshow pid
                            <> " killed by signal "
                            <> tshow sig
                            <> if coreDumped then " with core dumped" else ""
                    Just (Stopped sig) ->
                        logWarn
                            $  display
                            $  "Hadui dev process "
                            <> tshow pid
                            <> " stopped by signal "
                            <> tshow sig

    -- serve websockets in background threads
    let closeFdInParent conn = do
            -- this works with network-2.x
            let wsfd = fdSocket conn
            -- following works with network-3.x
            -- wsfd <- unsafeFdSocket conn

            -- always close the fd of socket in parent process, the socket
            -- will be shutdown when no fd kept open on it.
            -- the subprocess will hold the inherited fd open until it exits,
            -- and if no subprocess launched successfully, the socket will
            -- be shutdown righ away as the only fd in the parent process is
            -- closed here.
            closeFd $ Fd wsfd
    haduiListenWSC cfg closeFdInParent upstartHandler

    -- continue to serve http in main thread
    haduiServeHttp cfg (haduiProjectRoot devs) haduiResRoot

