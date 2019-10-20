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

module HaduiGHCi
    ( frontendPlugin
    , mustUIO
    )
where

import           RIO
import qualified RIO.Text                      as T
import qualified RIO.Text.Lazy                 as TL
import qualified Data.Text.Lazy.Encoding       as TLE

import           System.IO.Unsafe

import qualified GHC                           as GHC
import qualified HscTypes                      as GHC
import qualified ErrUtils                      as GHC
import qualified Panic                         as GHC
import qualified GhcMonad                      as GHC
import qualified GhcPlugins                    as GHC

import           Network.Socket                 ( mkSocket )
import qualified Network.WebSockets            as WS

import           UIO
import           HaduiRT


frontendPlugin :: GHC.FrontendPlugin
frontendPlugin = GHC.defaultFrontendPlugin { GHC.frontend = gfeMain }  where
    -- GHC frontend plugin entry point
    gfeMain flags args = do
        case args of
            [] -> pure ()
            _  -> GHC.throwGhcException
                $ GHC.CmdLineError "WebUI expects NO src file"
        case flags of
            [wsfdStr] -> case (readMaybe wsfdStr :: Maybe Int) of
                Nothing ->
                    GHC.throwGhcException $ GHC.CmdLineError "Invalid wsfd"
                Just wsfd -> (liftIO $ initUIO wsfd) >>= interactiveUI
            _ -> GHC.throwGhcException
                $ GHC.CmdLineError "WebUI expects single wsfd"

    initUIO :: Int -> IO UserInterfaceOutput
    initUIO wsfd = do
        uninitialized <- isEmptyMVar _globalUIO
        unless uninitialized $ error "initUIO reentrance ?!"

        (stackPrjRoot, cfg) <- loadConfig
        lo                  <- backendLogOptions cfg
        -- todo teardown 'lf' once the log target needs,
        -- not needed as we only log to stderr by far
        (lf, _ :: IO ())    <- newLogFunc lo
        mu                  <- newMVar ()
        wsc                 <- websocketFromFD wsfd
        let uio = UserInterfaceOutput { haduiProjectRoot = stackPrjRoot
                                      , haduiCommMutex   = mu
                                      , haduiWebSocket   = wsc
                                      , backendLogFunc   = lf
                                      }
        putMVar _globalUIO uio
        runUIO uio
            $  logInfo
            $  "hadui interactive frontend serving wsfd="
            <> (display $ tshow wsfd)
        return uio

_globalUIO :: MVar UserInterfaceOutput
{-# NOINLINE _globalUIO #-}
_globalUIO = unsafePerformIO newEmptyMVar


-- | every statement will be lifted by this function into IO monad for GHCi to execute.
-- this also very explicitly hints the expected monad type for all statements to eval.
mustUIO :: NFData a => UIO a -> IO ()
mustUIO m = do
    uio <- readMVar _globalUIO
    v   <- runUIO uio m
    _   <- pure $!! v -- force it to be executed
    pure ()


-- TODO this seems not invoked if we do handleSourceError, figure out if
--      it's appropriate to not do setLogAction at all.
uioLogCompilerOutput :: UserInterfaceOutput -> GHC.LogAction
uioLogCompilerOutput uio dflags reason severity srcSpan style msg = do
    -- TODO print to web front UI log with colors,
    --     and only print to backend in debug level.
    runUIO uio $ logDebug " *** not directing GHC log to front UI yet."
    GHC.defaultLogAction dflags reason severity srcSpan style msg

interactiveUI :: UserInterfaceOutput -> GHC.Ghc ()
interactiveUI uio = do
    -- obtain GHC session for the interaction course
    ghcSession <- GHC.reifyGhc return

    -- log to web UI
    GHC.setLogAction $ uioLogCompilerOutput uio

    -- make sure 'mustUIO' is in scope
    _ <- GHC.runDeclsWithLocation "<hadui-ghci-init>"
                                  1
                                  "import HaduiGHCi(mustUIO)"

    -- to allow literal Text/Int without explicit type anno
    _ <- GHC.runDeclsWithLocation "<hadui-ghci-init>" 1 "default (Text, Int)"

    runUIO uio $ uiLog $ DetailedMsg "hadui ready for project at: "
                                     (T.pack $ haduiProjectRoot uio)

    -- todo more semanticly diversified interpretions ?
    let !wsc = haduiWebSocket uio
        uioExecStmt :: Text -> GHC.Ghc ()
        uioExecStmt stmt = do
            _execResult <- GHC.execStmt
                ("mustUIO $\n" ++ T.unpack stmt)
                GHC.execOptions { GHC.execSourceFile = "<hadui-adhoc>"
                                , GHC.execLineNumber = 0
                                }
            -- TODO say sth about the result to web UI ?
            return ()

        interpretTextPacket :: TL.Text -> GHC.Ghc ()
        interpretTextPacket pkt = do
            let stmt = TL.toStrict pkt
                logGhcExc exc = do
                    let errDetails = tshow exc
                    runUIO uio $ uiLog $ DetailedErrorMsg "GHC error: "
                                                          errDetails
                logSrcError err = do
                    dynFlags <- GHC.getSessionDynFlags
                    let errDetails = T.pack $ unlines $ map
                            (GHC.showSDoc dynFlags)
                            (GHC.pprErrMsgBagWithLoc $ GHC.srcErrorMessages err)
                    runUIO uio $ uiLog $ DetailedErrorMsg
                        "The stmt has error: "
                        errDetails
            GHC.handleGhcException logGhcExc
                $ GHC.handleSourceError logSrcError
                $ uioExecStmt stmt

    -- serving ws forever
    let servWS :: IO ()
        servWS = do
            pkt <- WS.receiveDataMessage wsc
            case pkt of
                (WS.Text _bytes (Just pktText)) ->
                    GHC.reflectGhc (interpretTextPacket pktText) ghcSession
                (WS.Binary _bytes) -> do
                    runUIO uio
                        $ logError "hadui received binary packet from ws ?!"
                    WS.sendCloseCode wsc 1003 ("?!?" :: Text)
                _ -> do
                    runUIO uio $ logError
                        "hadui received unknown packet from ws ?!"
                    WS.sendCloseCode wsc 1003 ("?!?" :: Text)
            -- anyway we should continue receiving, even after close request sent, 
            -- we are expected to process a CloseRequest ctrl message from peer.
            servWS
    liftIO $ catch servWS $ \exc -> do
        case exc of
            WS.CloseRequest closeCode closeReason
                | closeCode == 1000 || closeCode == 1001
                -> runUIO uio $ logDebug "hadui ws closed normally."
                | otherwise
                -> runUIO uio
                    $  logError
                    $  "hadui ws closed with code "
                    <> display closeCode
                    <> " and reason ["
                    <> display (TLE.decodeUtf8 closeReason)
                    <> "]"
            WS.ConnectionClosed ->
                runUIO uio $ logDebug $ "hadui ws disconnected forcefully."
            _ -> do
                runUIO uio $ logError "hadui unexpected ws error"
                WS.sendCloseCode wsc 1003 ("?!?" :: Text)
        -- yet still try to receive ctrl msg back from peer
        servWS


websocketFromFD :: Int -> IO WS.Connection
websocketFromFD fd = do
    sock  <- mkSocket (fromIntegral fd)
    pconn <- WS.makePendingConnection sock
        $ WS.defaultConnectionOptions { WS.connectionStrictUnicode = True }
    WS.acceptRequest pconn

