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
    )
where

import           RIO
import qualified RIO.Text                      as T
import           RIO.FilePath
import           UnliftIO.Process

import qualified System.Directory              as D

import           Data.Yaml.Aeson


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

