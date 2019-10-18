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
{-# LANGUAGE QuasiQuotes #-}

module UIO
    ( UIO(..)
    , UserInterfaceOutput(..)
    , runUIO
    , uiLog
    )
where

import           RIO

import qualified Network.WebSockets            as WS

import qualified Data.Aeson                    as A
import           Data.Aeson.QQ


-- | The monad for User Interface Output
-- UIO is output only, conversely to IO (which stands for Input/Output),
-- user inputs shall be facilitated with a registry of 'MVar's,
-- those get filled with 'IoC' from UI widgets.
newtype UIO a = UIO { unUIO :: ReaderT UserInterfaceOutput IO a }
    deriving (Functor, Applicative, Monad, MonadIO,
        MonadReader UserInterfaceOutput, MonadThrow)

data UserInterfaceOutput = UserInterfaceOutput {
    haduiProjectRoot :: FilePath
    , haduiCommMutex :: MVar ()
    , haduiWebSocket :: WS.Connection

    , backendLogFunc :: !LogFunc
    }

instance HasLogFunc UserInterfaceOutput where
    logFuncL = lens backendLogFunc (\x y -> x { backendLogFunc = y })

runUIO :: MonadIO m => UserInterfaceOutput -> UIO a -> m a
runUIO uio (UIO (ReaderT f)) = liftIO (f uio)



uiLog :: Text -> UIO ()
uiLog msg = do
    uio <- ask
    liftIO $ withMVar
        (haduiCommMutex uio)
        \() -> WS.sendDataMessage (haduiWebSocket uio) $ WS.Text
            (A.encode [aesonQQ|{
"type": "msg"
, "msgText": #{msg}
}|]
            )
            Nothing


-- TODO define functions including 'uiLog' etc., through websocket.
