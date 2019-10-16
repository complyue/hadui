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
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LambdaCase #-}

module HaduiGHCi
    ( frontendPlugin
    , mustUIO
    )
where

import           RIO
import           RIO.FilePath

import           System.IO.Unsafe

import           GHC
import           Panic                   hiding ( trace )
import           GhcPlugins

import           Network.Socket                 ( mkSocket )
import qualified Network.WebSockets            as WS

import           HadUI

{-

quick test cmdl:

```shell
stack ghci --with-ghc ghc-ife --ghci-options '-fobject-code -e ":frontend HaduiGHCi" -ffrontend-opt 5' < /dev/null
```

-}

frontendPlugin :: FrontendPlugin
frontendPlugin = defaultFrontendPlugin { frontend = gfeMain }  where

    -- GHC frontend plugin entry point
    gfeMain flags args = do
        case args of
            [] -> pure ()
            _  -> throwGhcException $ CmdLineError "WebUI expects NO src file"
        case flags of
            [wsfdStr] -> case (readMaybe wsfdStr :: Maybe Int) of
                Nothing   -> throwGhcException $ CmdLineError "Invalid wsfd"
                Just wsfd -> initHC wsfd
            _ -> throwGhcException $ CmdLineError "WebUI expects single wsfd"

    initHC wsfd = do
        uninitialized <- liftIO $ isEmptyMVar _globalHC
        unless uninitialized $ throwGhcException $ UsageError
            "initHC reentrance ?!"

        mu <- liftIO $ newMVar ()

        -- TBD
        let uio = UserInterfaceOutput { backendLogFunc = undefined
                                      , haduiCommMutex = mu
                                      , uiWSFD         = wsfd
                                      }
        liftIO $ putMVar _globalHC uio

        -- make sure 'mustUIO' is in scope
        _ <- runDeclsWithLocation "<hadui-init>" 1 "import HaduiGHCi(mustUIO)"

        return ()


_globalHC :: MVar UserInterfaceOutput
{-# NOINLINE _globalHC #-}
_globalHC = unsafePerformIO newEmptyMVar


-- | every statement will be lifted by this function into IO monad for GHCi to execute.
-- this also very explicitly hints the expected monad type for all statements to eval.
mustUIO :: NFData a => UIO a -> IO ()
mustUIO m = do
    uio <- readMVar _globalHC
    v   <- runUIO uio m
    _   <- pure $!! v -- force it to be executed
    pure ()
