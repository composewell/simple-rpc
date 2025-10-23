{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE QuantifiedConstraints #-}

-- | Types and functions common to client and server.
--
module Simple.RPC.Types
    ( -- * Intermediate Representation (IR)
      IntermediateRep
    , toIntermediateRep
    , fromIntermediateRep

    -- ** IR Arrays
    , createArr
    , arrIndex
    , ensureNullArray

    -- ** IR Maps
    , createObj
    , objLookup

    -- * IR Serialization
    , toBinStream
    , toChunkStream
    , fromBinStream
    , irFromString
    , irToString

    -- * Symbols
    , RpcEval(..)
    , RpcMap
    , RpcSymbol(..)
    , createRpcMap
    , lookupRpcSymbol

    -- * Pretty Printing
    , showPair

    -- * Endpoint access config
    , Executable(..)
    , Username
    , SSHConfig
    , RunningConfig(..)
    , exec
    , onSSH
    , asUser
    , sudo

    -- * RPC
    , Runner

    ) where

--------------------------------------------------------------------------------
-- Imports
--------------------------------------------------------------------------------

import Data.Aeson (ToJSON, FromJSON, toJSON, fromJSON)
import Data.Function ((&))
import Data.Vector ((!?))
import Data.Word (Word8)
import Streamly.Data.Stream (Stream)

import qualified Data.Aeson as Json
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KeyMap
import qualified Data.Bifunctor
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString.Lazy.Char8 as BSLC
import qualified Data.HashMap.Strict as HM
import qualified Data.Vector as Vector
import qualified Streamly.Data.Array as Array
import qualified Streamly.Data.Stream as Stream

--------------------------------------------------------------------------------
-- Running Config
--------------------------------------------------------------------------------

-- XXX ServerSpec

-- | (server name, port)?
type SSHConfig = (String, String)

-- | Username for ssh access.
type Username = String

-- | RPC server executable name and version.
data Executable =
    Executable
        { executablePath :: FilePath
        , executableVersion :: String
        }

-- XXX CallSpec

-- | RPC server specification: server, port, username, executable location,
-- privileges to use etc.
data RunningConfig =
    RunningConfig
        { rcSSH :: Maybe SSHConfig
        , rcUser :: Maybe Username
        , rcSudo :: Bool
        , rcExe :: Executable
        }

-- We've changed the name of this from "using" to "exec". haskell-src-exts
-- fails to parse the keword "using". Did not explore this in depth.
-- XXX rename to serverExe
exec :: Executable -> RunningConfig
exec = RunningConfig Nothing Nothing False

-- XXX rename to onServer

-- | Modify the server name and port to use for ssh access.
onSSH :: SSHConfig -> RunningConfig -> RunningConfig
onSSH x rc = rc { rcSSH = Just x }

-- | Modify the ssh username to use for running the endpoint.
asUser :: Username -> RunningConfig -> RunningConfig
asUser x rc = rc { rcUser = Just x }

-- XXX rename to asSuperUser or asRoot

-- | Use root privileges for running the endpoint.
sudo :: Bool -> RunningConfig -> RunningConfig
sudo x rc = rc { rcSudo = x }

--------------------------------------------------------------------------------
-- Symbols
--------------------------------------------------------------------------------

-- XXX Call?

-- | Client side call representation of an RPC endpoint invocation. It
-- represents a local function that takes an endpoint name and IR of the input
-- args and returns IR of the output.
type Runner ir = String -> ir -> IO ir

-- XXX Serve or Service

-- | The server side representation of an RPC endpoint invocation. It consists
-- of an endpoint name and the corresponding function call. The function is the
-- RPC wrapper that takes an IR of args as input and returns an IR as output.
data RpcEval ir =
    RpcEval
        { symbol :: String
        , eval :: ir -> IO ir
        }

-- XXX RpcEndpoint

-- | Represents both the call side and the server side of an RPC endpoint.
data RpcSymbol ir typ =
    RpcSymbol
        { run :: typ                                -- raw
        , call :: Runner ir -> typ                  -- client
        , evaluator :: RpcEval ir                   -- server
        }

-- XXX Map instead of HashMap?
-- XXX rename to ServiceMap

-- | A map of symbol names and corresponding evals.
type RpcMap ir = HM.HashMap String (ir -> IO ir)

-- XXX serviceMapFromList

-- | Create a map from a list of symbol name, eval pairs.
createRpcMap :: [RpcEval ir] -> RpcMap ir
createRpcMap = HM.fromList . map (\x -> (symbol x, eval x))

-- XXX lookupService

-- | Lookup a symbol in a map.
lookupRpcSymbol :: String -> RpcMap ir -> Maybe (ir -> IO ir)
lookupRpcSymbol = HM.lookup

--------------------------------------------------------------------------------
-- Pretty printing
--------------------------------------------------------------------------------

-- | Format a tag and its value for pretty printing.
showPair :: String -> String -> String
showPair tag val = "[" ++ tag ++ "] " ++ val

--------------------------------------------------------------------------------
-- Serialized values
--------------------------------------------------------------------------------

type IntermediateRep = Json.Value

--------------------------------------------------------------------------------
-- Conerting Haskell values to/from IR
--------------------------------------------------------------------------------

toIntermediateRep :: ToJSON a => a -> IntermediateRep
toIntermediateRep = toJSON

fromIntermediateRep :: FromJSON a => IntermediateRep -> a
fromIntermediateRep val =
    case fromJSON val of
        Json.Error err -> error err
        Json.Success a -> a

--------------------------------------------------------------------------------
-- Arrays in IR
--------------------------------------------------------------------------------

-- | Decode a null array in IR as the supplied value. If the array is
-- not null then it is an error.
ensureNullArray :: IntermediateRep -> a -> a
ensureNullArray (Json.Array vec) act =
    if Vector.length vec == 0
    then act
    else error "Input is not an empty"
ensureNullArray _ _ = error "Value is not an array type"

-- | Retrieve the value at the given index in an IR array.
arrIndex :: FromJSON a => IntermediateRep -> Int -> a
arrIndex (Json.Array vec) i =
    case vec !? i of
        Nothing -> error $ "Could not find arg at position: " ++ show i
        Just val -> fromIntermediateRep val
arrIndex _ _ = error "Value is not an array type"

-- | Create an array in IR from a list of IR Haskell values.
createArr :: [IntermediateRep] -> IntermediateRep
createArr = Json.Array . Vector.fromList

--------------------------------------------------------------------------------
-- Key-value maps in IR
--------------------------------------------------------------------------------

-- XXX swap the arguments?

-- | Lookup a value corresponding to a key in the IR representation of a
-- key-value map.
objLookup :: FromJSON a => IntermediateRep -> String -> a
objLookup (Json.Object obj) kStr =
    case KeyMap.lookup (Key.fromString kStr) obj of
        Just val -> fromIntermediateRep val
        Nothing -> error $ "Could not find: " ++ kStr
objLookup _ _ = error "Value is not an object type"

-- | Convert (String, IR) key-value pairs to a map in IR.
createObj :: [(String, IntermediateRep)] -> IntermediateRep
createObj kv = Json.object $ map (Data.Bifunctor.first Key.fromString) kv

--------------------------------------------------------------------------------
-- Conversions of IR
--------------------------------------------------------------------------------

-- | Convert the Haskell representation of IR (i.e. Json) to a byte stream.
toBinStream :: IntermediateRep -> Stream IO Word8
toBinStream input =
    Stream.enumerateFromTo 0 (len - 1)
        & fmap (BSL.index bsl)

    where
    bsl = Json.encode input
    len = BSL.length bsl

-- TODO: Convert Bytestring to Array directly

-- | Convert the Haskell representation of IR (i.e. Json) to a stream of
-- arrays.
toChunkStream :: IntermediateRep -> Stream IO (Array.Array Word8)
toChunkStream input =
    toBinStream input
        & Array.chunksOf 1024

-- | Convert a byte stream of IR (i.e. Json) to its Haskell representation.
fromBinStream :: Stream IO Word8 -> IO IntermediateRep
fromBinStream input = do
    res <- Stream.toList input
    case Json.decode $ BSL.pack res of
        Nothing -> error "Unable to decode"
        Just x -> pure x

-- | Convert the Latin1 Char String representation of IR (i.e. Json) to
-- its Haskell representation.
irFromString :: String -> IntermediateRep
irFromString input = do
    case Json.decode $ BSLC.pack input of
        Nothing -> error $ "Unable to decode: " ++ input
        Just x -> x

-- | Convert the Haskell representation of IR (i.e. Json) to String.
irToString :: IntermediateRep -> String
irToString input = BSLC.unpack $ Json.encode input
