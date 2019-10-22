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
import qualified RIO.Text                      as T
import qualified RIO.Text.Lazy                 as TL
import           RIO.FilePath

import qualified Data.ByteString.Builder       as SB

import qualified System.Directory              as D
import           UnliftIO.Concurrent

import           Network.Socket
import qualified Network.WebSockets            as WS

import qualified Data.Aeson                    as A
import           Data.Aeson.QQ                  ( aesonQQ )

import qualified Snap.Core                     as Snap
import qualified Snap.Http.Server              as Snap
import qualified Snap.Util.FileServe           as Snap

import           System.Posix.Types
import           System.Posix.Process
import           System.Posix.IO

import           HaduiCfg
import           HaduiRT

import qualified Paths_hadui                   as Meta


main :: IO ()
main = do
    (stackPrjRoot, cfg) <- loadHaduiConfig

    dataDir             <- Meta.getDataDir
    let haduiResRoot = dataDir </> "web"
    D.doesDirectoryExist haduiResRoot >>= \case
        True -> return ()
        _    -> error "hadui web resource directory missing ?!"

    lo <- haduiBackendLogOpts cfg
    withLogFunc lo $ \lf ->
        let devApp = HaduiDevServer { haduiProjectRoot = stackPrjRoot
                                    , haduiConfig      = cfg
                                    , haduiDevLogFunc  = lf
                                    }
        in  runRIO devApp $ runDevServer haduiResRoot

data HaduiDevServer = HaduiDevServer {
    haduiProjectRoot :: FilePath
    , haduiConfig :: !HaduiConfig
    , haduiDevLogFunc :: !LogFunc
    }

instance HasLogFunc HaduiDevServer where
    logFuncL = lens haduiDevLogFunc (\x y -> x { haduiDevLogFunc = y })


runDevServer :: FilePath -> RIO HaduiDevServer ()
runDevServer haduiResRoot = do
    devs <- ask
    let cfg = haduiConfig devs

    logDebug $ "hadui using resource dir: [" <> fromString haduiResRoot <> "]"
    logInfo
        $  "hadui developing project at ["
        <> fromString (haduiProjectRoot devs)
        <> "]"
    logDebug $ "hadui with-ghc: \n  " <> display (withGHC cfg)
    logDebug $ "hadui ghci-options: \n" <> display
        (T.unlines $ ("  " <>) <$> ghciOptions cfg)
    logDebug $ "hadui ghc-options: \n" <> display
        (T.unlines $ ("  " <>) <$> ghcOptions cfg)

    let !ghcExecutable = T.unpack $ withGHC cfg
        !extraOpts     = RIO.concat
            (  [ ["--ghci-options", show opt] | opt <- ghciOptions cfg ]
            ++ [ ["--ghc-options", show opt] | opt <- ghcOptions cfg ]
            )
        upstartHandler conn = do
            wsfd <- unsafeFdSocket conn

            -- clear FD_CLOEXEC flag so it can be passed to subprocess
            setFdOption (Fd wsfd) CloseOnExec False

            -- launch `stack ghci` with hadui's GHC frontend to serve the ws
            pid <- forkProcess $ executeFile
                "/usr/bin/env"
                False
                (  [ "stack"
                   , "ghci"

                    -- TODO stack will ask through the tty if multiple executables
                    -- are defined in the project, hadui won't play well in this
                    -- case. file an issue with stack, maybe introduce a new cmdl
                    -- option to load all library modules with no question asked.

                    -- use designated GHC
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
                   , "-e \":frontend HaduiDev\" -ffrontend-opt " ++ show wsfd
                   ]
                ++ extraOpts -- opts from hadui.yaml
                )
                Nothing

            runRIO devs
                $  logDebug
                $  display
                $  "hadui started dev process pid: "
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
                            $  "hadui dev process "
                            <> tshow pid
                            <> " exited."
                    Just (Exited exitCode) ->
                        logError
                            $  display
                            $  "hadui dev process "
                            <> tshow pid
                            <> " exited with "
                            <> tshow exitCode
                    Just (Terminated sig coreDumped) ->
                        logWarn
                            $  display
                            $  "hadui dev process "
                            <> tshow pid
                            <> " killed by signal "
                            <> tshow sig
                            <> if coreDumped then " with core dumped" else ""
                    Just (Stopped sig) ->
                        logWarn
                            $  display
                            $  "hadui dev process "
                            <> tshow pid
                            <> " stopped by signal "
                            <> tshow sig

    -- serve websockets in background threads
    let closeFdInParent conn = do
            wsfd <- unsafeFdSocket conn
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

