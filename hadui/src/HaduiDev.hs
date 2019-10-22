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

module HaduiDev
    ( frontendPlugin
    )
where

import           RIO

import           GHC
import           Panic
import           GhcPlugins

import           HaduiRT


frontendPlugin :: FrontendPlugin
frontendPlugin = defaultFrontendPlugin { frontend = gfeMain }  where
    -- GHC frontend plugin entry point
    gfeMain flags args = do
        unless (null args) $ throwGhcException $ CmdLineError
            "HaduiDev expects NO src"
        case flags of
            [wsfdStr] -> case (readMaybe wsfdStr :: Maybe Int) of
                Nothing   -> throwGhcException $ CmdLineError "Invalid wsfd"
                Just wsfd -> haduiDevServer wsfd
            _ -> throwGhcException
                $ CmdLineError "HaduiDev expects single wsfd as arg"

