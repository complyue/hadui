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
    ( module RIO -- re-export RIO, so UIO can be used as prelude too

    -- monad stuff
    , UIO(..)
    , UserInterfaceOutput(..)
    , runUIO
    , mustUIO

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

import           HaduiRT


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
