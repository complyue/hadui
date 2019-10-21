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

module UIO
    ( module RIO -- re-export RIO,
    -- so UIO could be used as replacement prelude, too

    -- monad stuff
    , UIO(..)
    , UserInterfaceOutput(..)
    , runUIO

    -- comm functions
    , print
    , MsgToUI(..)
    , uiLog
    , uiClearLog
    , uiComm

    -- advanced harness
    , haduiExecStmt
    , haduiExecGhc
    , mustUIO
    )
where


import           RIO

import           HaduiMonad
import           HaduiRT


print :: Display a => a -> UIO ()
print v = uiLog $ TextMsg $ textDisplay v
