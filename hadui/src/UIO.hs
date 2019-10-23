{-# LANGUAGE NoImplicitPrelude #-}

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
    , withHaduiFront
    , haduiExecStmt
    , haduiExecGhc
    , mustUIO
    )
where

import           RIO

import           HaduiMonad
import           HaduiRT


-- | 'print' is just shorthand for 'uiLog . TextMsg . textDisplay'
print :: Display a => a -> UIO ()
print = uiLog . TextMsg . textDisplay

