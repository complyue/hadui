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
    ( HaduiConfig(..)
    , loadHaduiConfig
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
                                    True ->
                                        withBinaryFile haduiYamlFile ReadMode
                                            $ \h -> do -- TODO can this be point-free ?
                                                  bytes <- BL.hGetContents h
                                                  return $! decode1 bytes
                                        -- following won't work coz laziness
                                        -- (fmap decode1 . BL.hGetContents)
                                    -- parse empty dict for all defaults
                                    _ -> return $ decode1 "{}"
                            >>= \case
                                    Left yamlErr ->
                                        error
                                            $  "Error with hadui.yaml "
                                            <> yamlErr
                                    Right cfg -> return (stackPrjRoot, cfg)
                _ -> error "Can not determine stack project root."

data HaduiConfig = HaduiConfig {
    bindInterface :: Text
    , httpPort :: Int
    , wsPort :: Int
    , logLevel :: Text
    , withGHC :: Text
    , ghciOptions :: [Text]
    , ghcOptions :: [Text]
    , stackOptions :: [Text]
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
            .:? "with-ghc"
            .!= "ghc"
            <*> v
            .:? "ghci-options"
            .!= ["-fobject-code"]
            <*> v
            .:? "ghc-options"
            .!= []
            <*> v
            .:? "stack-options"
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


haduiGHCiCmdl :: HaduiConfig -> String -> [String] -> [String]
haduiGHCiCmdl cfg fePluginName feArgs =
    -- the cmdl allowing copy&paste to bash prompt
    --                                     trace
    -- (  "# --- begin hadui cmdl ---\n"
    -- <> T.pack (unwords [ "'" <> o <> "'" | o <- cmdl ])
    -- <> "\n# === end hadui cmdl ==="
    -- )
                                        cmdl  where
    !ghcExecutable = T.unpack $ withGHC cfg
    !cmdl =
        ["stack", "ghci"]
            ++ (T.unpack <$> stackOptions cfg)
            ++ [

-- TODO stack will ask through the tty if multiple executables
-- are defined in the project, hadui won't play well in this
-- case. file an issue with stack, maybe introduce a new cmdl
-- option to load all library modules with no question asked.

-- use designated GHC
                 "--with-ghc"
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

