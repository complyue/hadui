{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE QuasiQuotes #-}

module HaduiUtil
    ( wsSendText
    , wsSendData
    )
where

import           UIO

import qualified RIO.ByteString.Lazy           as BL
import qualified RIO.Vector.Storable           as VS

import qualified Data.ByteString.Unsafe        as B'
import qualified Data.Vector.Storable.Mutable  as VSM'

import qualified Network.WebSockets            as WS
import qualified Data.Aeson                    as A

import           Foreign


wsSendText :: (MonadIO m, A.ToJSON a) => WS.Connection -> a -> m ()
wsSendText wsc jsonCmd =
    -- 'A.encode' may crash the process if lazily called here
    liftIO $ WS.sendDataMessage wsc $ flip WS.Text Nothing $! (A.encode jsonCmd)

wsSendData
    :: forall m a
     . (MonadIO m, Storable a)
    => WS.Connection
    -> VS.MVector (PrimState IO) a
    -> m ()
wsSendData wsc arry = liftIO $ do
    let !itemSize    = sizeOf (undefined :: a)
        !(fptr, len) = VSM'.unsafeToForeignPtr0 arry
    -- we use unsafe coercion to ByteString here for zero-copy performance,
    -- it is still safe as the memory is only used during 'withForeignPtr'
    withForeignPtr fptr $ \ptr -> do
        bs <- B'.unsafePackCStringLen (castPtr ptr, len * itemSize)
        WS.sendDataMessage wsc $ WS.Binary $ BL.fromStrict bs

