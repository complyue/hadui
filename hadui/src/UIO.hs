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
    ( module RIO -- use RIO as prelude

    -- monad stuff
    , UIO(..)
    , UserInterfaceOutput(..)
    , runUIO

    -- log functions
    , print
    , MsgToUI(..)
    , uiLog
    , uiClearLog
    )
where

import           RIO

import qualified Network.WebSockets            as WS

import qualified Data.Aeson                    as A
import           Data.Aeson.QQ                  ( aesonQQ )


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

print :: Display a => a -> UIO ()
print v = uiLog $ TextMsg $ textDisplay v


data MsgToUI = TextMsg Text
    | DetailedMsg Text TheDetails
    | ErrorMsg Text
    | DetailedErrorMsg Text TheDetails
    | HtmlMsg Text
type TheDetails = Text

uiLog :: MsgToUI -> UIO ()
uiLog msg = do
    uio <- ask
    liftIO $ withMVar (haduiCommMutex uio) $ \() -> case msg of
        TextMsg msgText -> WS.sendDataMessage (haduiWebSocket uio) $ WS.Text
            (A.encode [aesonQQ|{
"type": "msg"
, "msgText": #{msgText}
}|]
            )
            Nothing
        DetailedMsg msgText msgDetails ->
            WS.sendDataMessage (haduiWebSocket uio) $ WS.Text
                (A.encode [aesonQQ|{
"type": "msg"
, "msgText": #{msgText}
, "msgDetails": #{msgDetails}
}|]
                )
                Nothing
        ErrorMsg errText -> WS.sendDataMessage (haduiWebSocket uio) $ WS.Text
            (A.encode [aesonQQ|{
"type": "msg"
, "msgText": #{errText}
, "msgType": "err-msg"
}|]
            )
            Nothing
        DetailedErrorMsg errText errDetails ->
            WS.sendDataMessage (haduiWebSocket uio) $ WS.Text
                (A.encode [aesonQQ|{
"type": "msg"
, "msgText": #{errText}
, "msgType": "err-msg"
, "msgDetails": #{errDetails}
}|]
                )
                Nothing

        -- more TBD
        _ -> pure ()


uiClearLog :: UIO ()
uiClearLog = do
    uio <- ask
    liftIO $ withMVar (haduiCommMutex uio) $ \() ->
        WS.sendDataMessage (haduiWebSocket uio) $ WS.Text
            (A.encode [aesonQQ|{
"type": "clear"
}|]
            )
            Nothing


-- TODO more uiXXX functions
