{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Main
    ( main
    )
where

import           RIO

import           System.Posix.Process

import           HaduiCfg


main :: IO ()
main = do
    (_stackPrjRoot, cfg) <- loadHaduiConfig

    executeFile "/usr/bin/env" False (haduiGHCiCmdl cfg "HaduiPub" []) Nothing
