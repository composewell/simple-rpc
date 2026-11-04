{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE RecordWildCards #-}

module Simple.RPC.Server
    ( mainWith
    , RpcMap
    , createRpcMap
    , invokeFunction
    ) where

--------------------------------------------------------------------------------
-- Imports
--------------------------------------------------------------------------------

import Control.Exception (throwIO)
import Data.Function ((&))
import Streamly.Internal.Unicode.String (str)
import System.Environment (getArgs)
import System.IO (hSetEncoding, stdin, stdout, utf8)

import qualified Streamly.Data.Array as Array
import qualified Streamly.Data.Fold as Fold
import qualified Streamly.Data.Stream as Stream
import qualified Streamly.FileSystem.Handle as FH

import Simple.RPC.Internal.Types

--------------------------------------------------------------------------------
-- Utils
--------------------------------------------------------------------------------

-- XXX To reduce execution delays we should have this running as a server. Or
-- at least make sure the executable is not swapped out.

-- | Given a RpcMap and a function name invoke the function. The function input
-- is supplied in serialized form on the stdin and the output is streamed in
-- serialized form to stdout.
--
-- The encoding of stdout is set to UTF-8, the client decodes the output as
-- UTF-8 independent of the locale of the server.
--
-- Raise an 'EndpointNotFound' exception if the map does not have the
-- function, and an 'InvalidInput' exception if the input is not valid JSON.
invokeFunction :: RpcMap IntermediateRep -> String -> IO ()
invokeFunction actions actionName = do
    -- XXX we should not buffer the input, it should be directly streamed to
    -- the function, otherwise we cannot support infinite or large input.

    -- Read the input before raising an exception for an unknown endpoint. A
    -- client writing the input to a process that has exited gets a broken
    -- pipe error instead of the exit code of the process.
    inputBytes <-
        FH.read stdin & Stream.fold (Fold.takeEndBy_ (== 10) Array.create)
    case lookupRpcSymbol actionName actions of
        Nothing -> throwIO (EndpointNotFound actionName)
        Just actFun -> do
            hSetEncoding stdout utf8
            let inputString = decodeUtf8Array inputBytes
            putStrLn $ showPair "SERVER" [str|#{actionName} #{inputString}|]
            r <- irFromArray inputBytes
            input <-
                case r of
                    Left err -> throwIO (InvalidInput err)
                    Right x -> pure x
            output <- actFun input
            putStrLn ""
            toChunkStream output & Stream.fold (FH.writeChunks stdout)

-- | The main function for the RPC endpoint executable. The first argument is
-- the version number. The behavior of the main function in the executable is
-- as follows:
--
-- * If no arguments are provided, it prints the version number in UTF-8
-- encoding, without a newline.
-- * If more than one argument is provided, it raises an 'InvalidUsage'
-- exception.
-- * If exactly one argument is provided, the argument is treated as the name
-- of the endpoint to be executed.
--
-- The RPC server executable implements several endpoints, it has a map of the
-- endpoint name to the function that implements the endpoint. If the endpoint
-- is not found in the map then it raises an 'EndpointNotFound' exception. If the endpoint is found then
-- the stdin of the RPC executable is supplied as input to the corresponding
-- function. The input is a serialized form (JSON) of the 'IntermediateRep'.
-- The output of the endpoint is emitted on stdout as serialized form of
-- 'IntermediateRep'.
mainWith :: String -> RpcMap IntermediateRep -> IO ()
mainWith version actions = do
    args <- getArgs
    case args of
        [] -> hSetEncoding stdout utf8 >> putStr version
        [actName] -> invokeFunction actions actName
        _ -> throwIO (InvalidUsage args)
