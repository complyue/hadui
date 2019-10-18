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
import           RIO.Text
import qualified RIO.Text.Lazy                 as TL
import qualified Data.Text.Lazy.Encoding       as TLE

import           System.IO.Unsafe

import           GHC                           as GHC
import           GHCi                          as GHCi
import           Panic                   hiding ( trace )
import           GhcPlugins              hiding ( trace
                                                , (<>)
                                                )

import           Network.Socket                 ( mkSocket )
import qualified Network.WebSockets            as WS

import qualified Data.Aeson                    as A
import           Data.Aeson.QQ

import           UIO
import           HaduiRT


frontendPlugin :: FrontendPlugin
frontendPlugin = defaultFrontendPlugin { frontend = gfeMain }  where
    -- GHC frontend plugin entry point
    gfeMain flags args = do
        case args of
            [] -> pure ()
            _  -> throwGhcException $ CmdLineError "WebUI expects NO src file"
        case flags of
            [wsfdStr] -> case (readMaybe wsfdStr :: Maybe Int) of
                Nothing   -> throwGhcException $ CmdLineError "Invalid wsfd"
                Just wsfd -> (liftIO $ initUIO wsfd) >>= interactiveUI
            _ -> throwGhcException $ CmdLineError "WebUI expects single wsfd"

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


uioLogCompilerOutput :: UserInterfaceOutput -> LogAction
uioLogCompilerOutput uio dflags reason severity srcSpan style msg = do
    -- TODO print to web front UI log
    runUIO uio $ logWarn " ** logging compiler output to web UI is to be impl"

    -- TODO only print to backend in debug level
    defaultLogAction dflags reason severity srcSpan style msg

interactiveUI :: UserInterfaceOutput -> Ghc ()
interactiveUI uio = do
    -- obtain compiler session
    hsc_env <- getSession

    -- log to web UI
    setLogAction $ uioLogCompilerOutput uio

    -- make sure 'mustUIO' is in scope
    _ <- runDeclsWithLocation "<hadui-ghci-init>" 1 "import HaduiGHCi(mustUIO)"

    -- to allow literal Text/Int without explicit type anno
    _ <- runDeclsWithLocation "<hadui-ghci-init>" 1 "default (Text, Int)"

    runUIO uio $ uiLog $ DetailedMsg "hadui ready for project at: "
                                     (pack $ haduiProjectRoot uio)

    -- todo more semanticly diversified interpretions ?
    let !wsc = haduiWebSocket uio
        uioExecStmt :: Text -> Ghc ()
        uioExecStmt stmt = do
-- TODO lineNo should start at -1 to match UI input, how to do that ?
            fhv <- compileExprRemote ("mustUIO $\n" ++ (unpack stmt))
            liftIO $ evalIO hsc_env fhv

        interpretTextPacket :: TL.Text -> Ghc ()
        interpretTextPacket pkt = do
            let stmt = TL.toStrict pkt
            gcatch (uioExecStmt stmt) $ \(exc :: GhcException) -> do
                let errDetails = tshow exc
                runUIO uio
                    $  logError
                    $  "hadui stmt exec error: "
                    <> (display errDetails)
                liftIO $ withMVar
                    (haduiCommMutex uio)
                    \() -> WS.sendDataMessage wsc $ WS.Text
                        (A.encode [aesonQQ|{
"type": "err"
, "errText": "unexpected error:"
, "errDetails": #{errDetails}
}|]
                        )
                        Nothing

    -- serving ws forever
    let servWS :: Ghc ()
        servWS = do
            pkt <- liftIO $ WS.receiveDataMessage wsc
            case pkt of
                (WS.Text _bytes (Just pktText)) -> interpretTextPacket pktText
                (WS.Binary _bytes             ) -> do
                    runUIO uio
                        $ logError
                        $ "hadui received binary packet from ws ?!"
                    liftIO $ WS.sendCloseCode wsc 1003 ("?!?" :: Text)
                _ -> do
                    runUIO uio $ logError
                        "hadui received unknown packet from ws ?!"
                    liftIO $ WS.sendCloseCode wsc 1003 ("?!?" :: Text)
            -- anyway we should continue receiving, even after close request sent, 
            -- we are expected to process a CloseRequest ctrl message from peer.
            servWS
    gcatch servWS $ \exc -> do
        liftIO $ case exc of
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
                liftIO $ WS.sendCloseCode wsc 1003 ("?!?" :: Text)
        -- yet still try to receive ctrl msg back from peer
        servWS


websocketFromFD :: Int -> IO WS.Connection
websocketFromFD fd = do
    sock  <- mkSocket (fromIntegral fd)
    pconn <- WS.makePendingConnection sock
        $ WS.defaultConnectionOptions { WS.connectionStrictUnicode = True }
    WS.acceptRequest pconn

