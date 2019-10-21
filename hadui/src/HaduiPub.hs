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

module HaduiPub
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
            "HaduiPub expects NO src"
        unless (null flags) $ throwGhcException $ CmdLineError
            "HaduiPub expects NO arg"

        haduiPubServer

