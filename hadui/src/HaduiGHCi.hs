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
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE BangPatterns #-}

module HaduiGHCi
    ( frontendPlugin
    )
where

import           RIO
import qualified RIO.Text                      as T
import qualified RIO.Text.Lazy                 as TL
import qualified Data.Text.Lazy.Encoding       as TLE

import qualified GHC                           as GHC
import qualified HscTypes                      as GHC
import qualified ErrUtils                      as GHC
import qualified Panic                         as GHC
import qualified GhcMonad                      as GHC
import qualified GhcPlugins                    as GHC

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


interactiveUI :: UserInterfaceOutput -> GHC.Ghc ()
interactiveUI uio = do
    runUIO uio $ uiLog $ DetailedMsg "hadui serving project at: "
                                     (T.pack $ haduiProjectRoot uio)

    -- obtain GHC session for the interaction course
    ghcSession <- GHC.reifyGhc return

    -- to have GHC errors caught and displayed in web UI
    let logGhcExc exc = do
            let errDetails = tshow exc
            runUIO uio $ uiLog $ DetailedErrorMsg "GHC error: " errDetails
        logSrcError err = do
            dynFlags <- GHC.getSessionDynFlags
            let errDetails = T.pack $ unlines $ map
                    (GHC.showSDoc dynFlags)
                    (GHC.pprErrMsgBagWithLoc $ GHC.srcErrorMessages err)
            runUIO uio $ uiLog $ DetailedErrorMsg "Source error: " errDetails
        execGhc :: GHC.Ghc () -> GHC.Ghc ()
        execGhc action =
            GHC.handleGhcException logGhcExc
                $ GHC.handleSourceError logSrcError action
        execStmt :: Text -> GHC.Ghc ()
        execStmt stmt = execGhc $ do
            _execResult <- GHC.execStmt
                ("mustUIO $\n" ++ T.unpack stmt)
                GHC.execOptions { GHC.execSourceFile = "<hadui-adhoc>"
                                , GHC.execLineNumber = 0
                                }
            -- TODO say sth about the result to web UI ?
            return ()

    execGhc $ do
        -- make sure 'UIO' is in scope
        GHC.getContext
            >>= GHC.setContext
            .   ((GHC.IIDecl $ GHC.simpleImportDecl $ GHC.mkModuleName "UIO") :)
        _ <- GHC.runDecls "default (Text,Int)"
        -- to allow literal Text/Int without explicit type anno
        return ()

    -- every time a text packet received, just execute it as Haskell statement
    let !wsc = haduiWebSocket uio
        interpretTextPacket :: TL.Text -> GHC.Ghc ()
        interpretTextPacket pkt = let stmt = TL.toStrict pkt in execStmt stmt

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
