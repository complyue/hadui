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

module HadUI
    ( UIO(..)
    , UserInterfaceOutput(..)
    , runUIO
    , uiLog
    )
where

import           RIO
import           RIO.FilePath

import           Network.Socket                 ( mkSocket )
import qualified Network.WebSockets            as WS


-- | The monad for User Interface Output
-- UIO is output only, conversely to IO (which stands for Input/Output),
-- user inputs shall be facilitated with a registry of 'MVar's,
-- those get filled with 'IoC' from UI widgets.
newtype UIO a = UIO { unUIO :: ReaderT UserInterfaceOutput IO a }
    deriving (Functor, Applicative, Monad, MonadIO,
        MonadReader UserInterfaceOutput, MonadThrow)

data UserInterfaceOutput = UserInterfaceOutput {
    backendLogFunc :: !LogFunc,

    haduiCommMutex :: MVar ()

    , uiWSFD :: Int -- TODO no need to save it, remove after PoC done
    -- , uiWebsocket :: WS.Connection
}

instance HasLogFunc UserInterfaceOutput where
    logFuncL = lens backendLogFunc (\x y -> x { backendLogFunc = y })

runUIO :: MonadIO m => UserInterfaceOutput -> UIO a -> m a
runUIO uio (UIO (ReaderT f)) = liftIO (f uio)


websocketFromFD :: Int -> IO WS.Connection
websocketFromFD fd = do
    sock  <- mkSocket (fromIntegral fd)
    pconn <- WS.makePendingConnection sock
        $ WS.defaultConnectionOptions { WS.connectionStrictUnicode = True }
    WS.acceptRequest pconn



uiLog :: IsString text => text -> UIO ()
uiLog msg = undefined

-- TODO define functions including 'uiLog' etc., through websocket.
