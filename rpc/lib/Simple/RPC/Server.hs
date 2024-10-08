{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE RecordWildCards #-}

module Simple.RPC.Server
    (
    -- Rpc helpers
      rpcExport
    , rpcExportLenient
    , RpcSymbol(..)

    -- Main function
    , mainWith
    ) where

--------------------------------------------------------------------------------
-- Imports
--------------------------------------------------------------------------------

import System.IO (stdout, stdin)
import Data.Function ((&))
import Data.List (find)
import System.Environment (getArgs)
import Streamly.Internal.Unicode.String (str)

import qualified Streamly.Data.Stream as Stream
import qualified Streamly.Data.Fold as Fold
import qualified Streamly.Unicode.Stream as Unicode
import qualified Streamly.FileSystem.Handle as FH hiding (read)
import qualified Streamly.Internal.FileSystem.Handle as FH (read)

import Simple.RPC.TH
import Simple.RPC.Types

--------------------------------------------------------------------------------
-- Utils
--------------------------------------------------------------------------------

mainWith :: String -> [RpcEval IntermediateRep] -> IO ()
mainWith version actions = do
    args <- getArgs
    case args of
        [] -> putStr version
        [actName] -> runner actName
        _ -> error [str|Usage: <executable> [<action>]|]

    where

    runner actionName =
        case find ((== actionName) . symbol) actions of
            Nothing -> error [str|#{actionName} not found|]
            Just (RpcEval actName actFun) -> do
                inputString <-
                    FH.read stdin
                        & Unicode.decodeUtf8'
                        & Stream.fold (Fold.takeEndBy_ (== '\n') Fold.toList)
                let input = irFromString inputString
                printWith "RPC" [str|#{actName} #{inputString}|]
                output <- actFun input
                putStrLn ""
                toBinStream output & Stream.fold (FH.write stdout)
