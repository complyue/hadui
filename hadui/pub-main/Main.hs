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

module Main
    ( main
    )
where

import           RIO
import qualified RIO.Text                      as T

import           System.Posix.Process

import           HaduiCfg


main :: IO ()
main = do
    (_stackPrjRoot, cfg) <- loadHaduiConfig

    let !ghcExecutable = T.unpack $ withGHC cfg
        !extraOpts     = concat
            (  [ ["--ghci-options", show opt] | opt <- ghciOptions cfg ]
            ++ [ ["--ghc-options", show opt] | opt <- ghcOptions cfg ]
            )

    executeFile
        "/usr/bin/env"
        False
        (  [ "stack"
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
           , "-e \":frontend HaduiPub\""
           ]
        ++ extraOpts -- opts from hadui.yaml
        )
        Nothing
