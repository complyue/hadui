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
    prj <- loadHaduiConfig

    executeFile "/usr/bin/env" False (haduiGHCiCmdl prj "HaduiPub" []) Nothing
