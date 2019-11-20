{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE QuasiQuotes #-}

module HaduiUtil
    ( wsSendJson
    , wsSendData
    )
where

import           UIO

import qualified RIO.ByteString.Lazy           as BL
import qualified RIO.Vector.Storable           as VS
import qualified RIO.Vector.Storable.Unsafe    as VS'

import qualified Data.ByteString.Unsafe        as B'

import qualified Network.WebSockets            as WS
import qualified Data.Aeson                    as A

import           Foreign


wsSendJson :: (MonadIO m, A.ToJSON a) => WS.Connection -> a -> m ()
wsSendJson wsc jsonCmd =
    liftIO
        $  WS.sendDataMessage wsc
        $  flip WS.Text Nothing
    -- 'A.encode' may crash the process if lazily called here
        $! (A.encode jsonCmd)


wsSendData
    :: forall m a
     . (MonadIO m, Storable a)
    => WS.Connection
    -> VS.Vector a
    -> m ()
wsSendData wsc arry = liftIO $ withForeignPtr fptr $ \ptr -> do
    -- we use unsafe coercion to ByteString here for zero-copy performance,
    -- it is safe as long as the caller is not modifying it concurrently,
    -- which obviously is not expected here.
    bs <- B'.unsafePackCStringLen (castPtr ptr, len * itemSize)
    WS.sendDataMessage wsc $ WS.Binary $ BL.fromStrict bs
  where
    !itemSize    = sizeOf (undefined :: a)
    -- it's safe to use 'unsafeToForeignPtr0' as we never write to it here
    !(fptr, len) = VS'.unsafeToForeignPtr0 arry

