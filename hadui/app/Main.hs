{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase #-}

module Main
    ( main
    )
where

import           RIO
import           RIO.FilePath
import qualified System.Directory              as D

import           HaduiRT

import qualified Paths_hadui                   as Meta


main :: IO ()
main = do
    (stackPrjRoot, cfg) <- loadConfig

    dataDir             <- Meta.getDataDir
    let haduiResRoot = dataDir </> "web"
    D.doesDirectoryExist haduiResRoot >>= \case
        True -> return ()
        _    -> error "hadui web resource directory missing ?!"

    lo <- backendLogOptions cfg
    withLogFunc lo $ \lf ->
        let haduiApp = HaduiFirstProcess { stackProjectRoot  = stackPrjRoot
                                         , haduiResourceRoot = haduiResRoot
                                         , haduiConfig       = cfg
                                         , appLogFunc        = lf
                                         }
        in  runRIO haduiApp runHadUI
