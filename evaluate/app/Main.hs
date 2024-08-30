{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Main (main) where

--------------------------------------------------------------------------------
-- Imports
--------------------------------------------------------------------------------

import System.Environment (getArgs)

import Data.Aeson
import Simple.RPC.Types
import Simple.RPC.Client

--------------------------------------------------------------------------------
-- Types
--------------------------------------------------------------------------------

data SSHConfigRec = SSHConfigRec { toSSHConfig :: SSHConfig }

instance FromJSON SSHConfigRec where
    parseJSON =
        withObject "SSHConfigRec" $ \v -> do
            ipAddr <- v .: "ip_addr"
            port <- v .: "port"
            pure $ SSHConfigRec (ipAddr, port)

instance FromJSON Executable where
    parseJSON =
        withObject "Executable" $ \v -> do
            path <- v .: "path"
            version <- v .: "version"
            pure $ Executable path version

instance FromJSON RunningConfig where
    parseJSON =
        withObject "RunningConfig" $ \v -> do
            ssh <- v .:? "ssh"
            user <- v .:? "username"
            exe <- v .: "executable"
            pure $ RunningConfig (toSSHConfig <$> ssh) user exe

data Action =
    Action
      { aEndpoint :: String
      , aArguments :: [Value]
      }

instance FromJSON Action where
    parseJSON =
        withObject "Action" $ \v -> do
            endpoint <- v .: "endpoint"
            args <- v .: "arguments"
            pure $ Action endpoint args

data ActionItem =
    ActionItem
        { aiRunningConfig :: RunningConfig
        , aiActions :: [Action]
        }

instance FromJSON ActionItem where
    parseJSON =
        withObject "ActionItem" $ \v -> do
            conf <- v .: "config"
            actions <- v .: "actions"
            pure $ ActionItem conf actions

type ActionPlan = [ActionItem]

readJsonFile :: FilePath -> IO ActionPlan
readJsonFile fp = do
    eres <- eitherDecodeFileStrict fp
    case eres of
        Left err -> error err
        Right val -> pure val

executeActionPlan :: ActionPlan -> IO ()
executeActionPlan plan =
    flip mapM_ plan $ \item -> do
        let runningConfig = (aiRunningConfig item)
        flip mapM_ (aiActions item) $ \action -> do
            with runningConfig (aEndpoint action) (toJSON (aArguments action))

main :: IO ()
main = do
    args <- getArgs
    case args of
        [actionFilePath] -> readJsonFile actionFilePath >>= executeActionPlan
        _ -> error "Exactly one argument is accepted"
