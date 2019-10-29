{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE QuasiQuotes #-}

-- | Hadui runtime
module HaduiMonad
    ( UIO(..)
    , UserInterfaceOutput(..)
    , runUIO
    , mustUIO
    , initUIO
    , withHaduiFront
    )
where

import           RIO

import           System.IO.Unsafe
import           Data.Dynamic                   ( Dynamic(..) )

import qualified GHC
import qualified GhcMonad                      as GHC

import qualified Network.WebSockets            as WS

import           HaduiCfg


-- | The monad for User Interface Output
-- UIO is output only, conversely to IO (which stands for Input/Output),
-- user inputs shall be facilitated with a registry of 'MVar's,
-- those get filled with 'IoC' from UI widgets.
newtype UIO a = UIO { unUIO :: ReaderT UserInterfaceOutput IO a }
    deriving (Functor, Applicative, Monad, MonadIO,
        MonadReader UserInterfaceOutput, MonadThrow)

-- | env of the UIO monad        
data UserInterfaceOutput = UserInterfaceOutput {
    -- | root directory of the stack project of matter
    haduiProjectRoot :: !FilePath

    -- | configuration loaded from `hadui.yaml` at project root
    , haduiConfig :: !HaduiConfig

    -- | arbitrary state managed by convention of the project
    , haduiAppData :: !(MVar (Maybe Dynamic))

    -- | Global Interpreter Lock similar to Python's, but serializes
    -- executions of ws packets only, a single ws packet can trigger
    -- fully fledged concurrency and parallelism in contrast to Python.
    --
    -- If a UIO action starts concurrent compution threads, and such
    -- a thread shall comm back to its originating ws, it must save
    -- the contextual websocket in GIL atm it's started.
    , haduiGIL :: !(MVar  WS.Connection)

    -- | the underlying log function to implement rio's HasLogFunc
    , haduiBackendLogFunc :: !LogFunc

    -- | GHC session used to execute statements by dynamic compilation
    , haduiGhcSession :: !GHC.Session
    }

instance MonadUnliftIO UIO where
    askUnliftIO = UIO $ ReaderT $ \r ->
        withUnliftIO $ \u ->
            return (UnliftIO (unliftIO u . flip runReaderT r . unUIO))

instance PrimMonad UIO where
    type PrimState UIO = PrimState IO
    primitive = UIO . ReaderT . const . primitive

instance HasLogFunc UserInterfaceOutput where
    logFuncL = lens haduiBackendLogFunc (\x y -> x { haduiBackendLogFunc = y })


-- | Run a 'UIO' action within any 'MonadIO' given a 'uio' env.
runUIO :: MonadIO m => UserInterfaceOutput -> UIO a -> m a
runUIO uio (UIO (ReaderT f)) = liftIO (f uio)


-- | Every statement being executed dynamically by Hadui, is unlifted
-- with this function into IO monad under the hood.
--
-- This very explicitly hints the expected monad type in the dynamic
-- compilation of such statements.
mustUIO :: UIO a -> IO ()
mustUIO m = do
    uio <- readIORef _globalUIO
    !_v <- runUIO uio m -- force it to be evaluated
    pure ()


_globalUIO :: IORef UserInterfaceOutput
{-# NOINLINE _globalUIO #-}
_globalUIO = unsafePerformIO $ newIORef undefined


-- | Initialize global UIO context with the calling Ghc Monad
initUIO :: GHC.Ghc UserInterfaceOutput
initUIO = do
    ghcSession <- GHC.reifyGhc return
    uio        <- liftIO $ do
        (stackPrjRoot, cfg) <- loadHaduiConfig
        appData             <- newMVar Nothing
        gil                 <- newEmptyMVar
        lo                  <- haduiBackendLogOpts cfg
        -- may need to teardown 'lf' on process exit, once the log target
        -- needs that, not needed as far as we only log to stderr.
        (lf, _ :: IO ())    <- newLogFunc lo
        return UserInterfaceOutput { haduiProjectRoot    = stackPrjRoot
                                   , haduiConfig         = cfg
                                   , haduiAppData        = appData
                                   , haduiGIL            = gil
                                   , haduiBackendLogFunc = lf
                                   , haduiGhcSession     = ghcSession
                                   }

    -- make module 'UIO' in scope implicitly
    GHC.getContext
        >>= GHC.setContext
        .   ((GHC.IIDecl $ GHC.simpleImportDecl $ GHC.mkModuleName "UIO") :)
    -- to allow string and number literals without explicit type anno
    _ <- GHC.runDecls "default (Text,Int)"

    -- XXX this does not work, have to use -fbreak-on-error on launching cmdl
    --
    -- break on error/exception to print error location for debuggability
    -- dynFlags <- GHC.getSessionDynFlags
    -- let dynFlags' = dynFlags & GHC.setGeneralFlag' GHC.Opt_BreakOnError
    --             -- stop only on uncaught exceptions with mere above,
    --             -- following enables stop on any exception:
    --             -- . GHC.setGeneralFlag' GHC.Opt_BreakOnException
    -- _ <- GHC.setSessionDynFlags dynFlags'

    liftIO $ writeIORef _globalUIO uio
    return uio


-- | Execute a UIO action with the specified ws for current context.
--
-- Note this is designed to be called from forked threads by wsc actions.
-- The wsc argument to use here is normally captured from 'haduiGIL' atm
-- the calling thread is started.
--
-- NEVER call this from threads directly triggered by UI, or it's deadlock.
withHaduiFront :: WS.Connection -> UIO a -> UIO a
withHaduiFront wsc action = do
    uio <- ask
    let gil = haduiGIL uio
    bracket_ (liftIO $ putMVar gil wsc) (liftIO $ takeMVar gil) action

