{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE RecordWildCards #-}

module Simple.RPC.Server
    (
      rpcExport
    , mainWith

    -- XXX Experimental
    , rpcify
    -- XXX this is re-exported
    , RpcSymbol(..)
    ) where

--------------------------------------------------------------------------------
-- Imports
--------------------------------------------------------------------------------

import Data.Function ((&))
import Simple.RPC.Internal.TH (rpcExport, rpcify)
import Streamly.Internal.Unicode.String (str)
import System.Environment (getArgs)
import System.IO (stdout, stdin)

import qualified Streamly.Data.Fold as Fold
import qualified Streamly.Data.Stream as Stream
import qualified Streamly.FileSystem.Handle as FH hiding (read)
import qualified Streamly.Internal.FileSystem.Handle as FH (read)
import qualified Streamly.Unicode.Stream as Unicode

import Simple.RPC.Types
    ( RpcSymbol(..), RpcMap, IntermediateRep, lookupRpcSymbol, irFromString
    , toBinStream, showPair)

--------------------------------------------------------------------------------
-- Utils
--------------------------------------------------------------------------------

-- XXX To reduce execution delays we should have this running as a server.

-- | The main function for the RPC endpoint executable. The first argument is
-- the version number. The behavior of the main function in the executable is
-- as follows:
--
-- * If no arguments are provided, it prints the version number.
-- * If more than one argument is provided, it is an error.
-- * If exactly one argument is provided, the argument is treated as the name
-- of the endpoint to be executed.
--
-- The RPC server executable implements several endpoints, it has a map of the
-- endpoint name to the function that implements the endpoint. If the endpoint
-- is not found in the map then it is an error. If the endpoint is found then
-- the stdin of the RPC executable is supplied as input to the corresponding
-- function. The input is a serialized form (JSON) of the 'IntermediateRep'.
-- The output of the endpoint is emitted on stdout as serialized form of
-- 'IntermediateRep'.
mainWith :: String -> RpcMap IntermediateRep -> IO ()
mainWith version actions = do
    args <- getArgs
    case args of
        [] -> putStr version
        [actName] -> runner actName
        _ -> error [str|Usage: <executable> [<action>]|]

    where

    runner actionName =
        case lookupRpcSymbol actionName actions of
            Nothing -> error [str|#{actionName} not found|]
            Just actFun -> do
                -- XXX we should not buffer the input, it should be directly
                -- streamed to the function, otherwise we cannot support
                -- infinite or large input.
                inputString <-
                    FH.read stdin
                        & Unicode.decodeUtf8'
                        & Stream.fold (Fold.takeEndBy_ (== '\n') Fold.toList)
                let input = irFromString inputString
                putStrLn $ showPair "SERVER" [str|#{actionName} #{inputString}|]
                output <- actFun input
                putStrLn ""
                toBinStream output & Stream.fold (FH.write stdout)
