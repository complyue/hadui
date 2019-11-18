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
{-# LANGUAGE QuasiQuotes #-}

-- | hadui configuration
module HaduiCfg
    ( HaduiProject(..)
    , HaduiConfig(..)
    , loadHaduiConfig
    , resolveHaduiResRoots
    , haduiBackendLogOpts
    , haduiGHCiCmdl
    )
where

import           RIO
import qualified RIO.Text                      as T
import qualified RIO.ByteString.Lazy           as BL
import           RIO.FilePath

import           UnliftIO.Process

import qualified System.Directory              as D

import           Data.YAML


loadHaduiConfig :: IO HaduiProject
loadHaduiConfig = do
    prjRoot         <- D.getCurrentDirectory
    isNixProject_   <- D.doesFileExist $ prjRoot </> "shell.nix"
    isCabalProject_ <- D.doesFileExist $ prjRoot </> "cabal.project"
    isStackProject_ <- D.doesFileExist $ prjRoot </> "stack.yaml"
    if not (isNixProject_ || isCabalProject_ || isStackProject_)
        then error "Hadui should run from a Haskell project dir!"
        else
            let haduiYamlFile = prjRoot </> "hadui.yaml"
            in  D.doesFileExist haduiYamlFile
                >>= \case
                        True -> withBinaryFile haduiYamlFile ReadMode $ \h ->
                            do -- TODO can this be point-free ?
                                bytes <- BL.hGetContents h
                                return $! decode1 bytes
                            -- following won't work coz laziness
                            -- (fmap decode1 . BL.hGetContents)
                        -- parse empty dict for all defaults
                        _ -> return $ decode1 "{}"
                >>= \case
                        Left yamlErr ->
                            error $ "Error with hadui.yaml " <> (show yamlErr)
                        Right cfg -> return $ HaduiProject prjRoot
                                                           isNixProject_
                                                           isCabalProject_
                                                           isStackProject_
                                                           cfg


resolveHaduiResRoots :: [Text] -> IO [FilePath]
resolveHaduiResRoots = mapM $ \pkg -> do
    (ExitSuccess, outBytes, "") <- readProcessWithExitCode
        "/usr/bin/env"
        [ "stack"
        , "exec"
        , "ghc-pkg"
        , "--"
        , "field"
        , "--simple-output"
        , T.unpack (pkg)
        , "data-dir"
        ]
        ""
    let pkgDataDir = (T.unpack . T.strip . T.pack) outBytes
    return $ pkgDataDir </> "web"


data HaduiProject = HaduiProject {
    projectRoot :: FilePath
    , isNixProject :: Bool
    , isCabalProject :: Bool
    , isStackProject :: Bool
    , haduiCfg :: HaduiConfig
} deriving (Eq,Show )

data HaduiConfig = HaduiConfig {
    bindInterface :: Text
    , httpPort :: Int
    , wsPort :: Int
    , logLevel :: Text
    , overlayPackages ::[Text]
    , ghciOptions :: [Text]
    , ghcOptions :: [Text]
} deriving (Eq,Show )

instance FromYAML HaduiConfig where
    parseYAML = withMap "HaduiConfig" $ \v ->
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
            .:? "overlay"
            .!= []
            <*> v
            .:? "ghci-options"
            .!= ["-fobject-code"]
            <*> v
            .:? "ghc-options"
            .!= []


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


-- stack tries to expose all artifacts from the underlying Haskell
-- project to GHCi, while cabal-install is rather conservative, so
-- we prefer stack over cabal here, as we are more application
-- oriented than library oriented. assuming those library users
-- who favor cabal-install over stack will not have `stack.yaml`
-- defined for the interactive Hadui project, and should enjoy
-- crafting a `.ghci` script to control artifact exposure.
-- see: https://github.com/haskell/cabal/issues/5374
haduiGHCiCmdl :: HaduiProject -> String -> [String] -> [String]
haduiGHCiCmdl prj fePluginName feArgs =
    -- the cmdl allowing copy&paste to bash prompt
    --                                     trace
    -- (  "# --- begin Hadui cmdl ---\n"
    -- <> T.pack (unwords [ "'" <> o <> "'" | o <- cmdl ])
    -- <> "\n# === end Hadui cmdl ==="
    -- )
                                        cmdl  where
    !cfg  = haduiCfg prj
    !cmdl = if (isStackProject prj)
        then
-- Stack project
            ["stack", "ghci"]
            ++ [

    -- TODO stack will ask through the tty if multiple executables
    -- are defined in the project, Hadui won't play well in this
    -- case. file an issue with stack, maybe introduce a new cmdl
    -- option to load all library modules with no question asked.

    -- use UIO which reexports RIO as prelude
                 "--ghc-options"
               , "-XNoImplicitPrelude"

    -- really hope that Haskell the language unify the string
    -- types (with utf8 seems the norm) sooner than later
               , "--ghc-options"
               , "-XOverloadedStrings"

    -- to allow literal Text/Int without explicit type anno
               , "--ghc-options"
               , "-XExtendedDefaultRules"

    -- to stop on uncaught errors
               , "--ghci-options"
               , "-fbreak-on-error"

    -- the frontend trigger
               , "--ghci-options"
               , "-e \":frontend " ++ fePluginName ++ "\""
               ]
            ++ concat
                   (  [ ["--ghci-options", "-ffrontend-opt " ++ fea]
                      | fea <- feArgs
                      ]
                   ++ [ ["--ghci-options", T.unpack opt]
                      | opt <- ghciOptions cfg
                      ]
                   ++ [ ["--ghc-options", T.unpack opt]
                      | opt <- ghcOptions cfg
                      ]
                   )
        else if (isCabalProject prj)
            then
-- Cabal project
                ["cabal", "v2-repl"]
                ++ [

    -- TODO stack will ask through the tty if multiple executables
    -- are defined in the project, Hadui won't play well in this
    -- case. file an issue with stack, maybe introduce a new cmdl
    -- option to load all library modules with no question asked.

    -- use UIO which reexports RIO as prelude
                     "--repl-options"
                   , "-XNoImplicitPrelude"

    -- really hope that Haskell the language unify the string
    -- types (with utf8 seems the norm) sooner than later
                   , "--repl-options"
                   , "-XOverloadedStrings"

    -- to allow literal Text/Int without explicit type anno
                   , "--repl-options"
                   , "-XExtendedDefaultRules"

    -- to stop on uncaught errors
                   , "--repl-options"
                   , "-fbreak-on-error"

    -- the frontend trigger
                   , "--repl-options"
                   , "-e \":frontend " ++ fePluginName ++ "\""
                   ]
                ++ concat
                       (  [ ["--repl-options", "-ffrontend-opt " ++ fea]
                          | fea <- feArgs
                          ]
                       ++ [ ["--repl-options", T.unpack opt]
                          | opt <- ghciOptions cfg
                          ]
                       ++ [ ["--repl-options", T.unpack opt]
                          | opt <- ghcOptions cfg
                          ]
                       )
            else
-- Nix project or others
                [ "ghci"

    -- use UIO which reexports RIO as prelude
                , "-XNoImplicitPrelude"

    -- really hope that Haskell the language unify the string
    -- types (with utf8 seems the norm) sooner than later
                , "-XOverloadedStrings"

    -- to allow literal Text/Int without explicit type anno
                , "-XExtendedDefaultRules"

    -- to stop on uncaught errors
                , "-fbreak-on-error"

    -- the frontend trigger
                , "-e \":frontend " ++ fePluginName ++ "\""
                ]
                ++ [ "-ffrontend-opt " ++ fea | fea <- feArgs ]
                ++ [ T.unpack opt | opt <- ghciOptions cfg ]
                ++ [ T.unpack opt | opt <- ghcOptions cfg ]

