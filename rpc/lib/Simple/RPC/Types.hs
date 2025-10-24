{-# LANGUAGE ImpredicativeTypes #-}
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
    , RunningConfig(..)
    , defaultConfig
    , setImage
    , setImageFlags
    , setRemote
    , setUser
    , useSudo
    , setLogger

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

-- | RPC server executable name and version.
data Executable =
    Executable
        { executablePath :: FilePath
        , executableVersion :: String
        }

-- XXX CallSpec
-- XXX Currently user is specified at two places user@hostname and rcUser. We
-- need to have a single mechanism.

-- | RPC server specification: server, port, username, executable location,
-- privileges to use etc.
data RunningConfig =
    RunningConfig
        { rcRemote :: Maybe (String, String)
        , rcUser :: Maybe String
        , rcSudo :: Bool
        , rcImage :: Executable
        , rcImageFlags :: String
        , rcLogger :: String -> IO ()
        }

-- XXX Use the default image as the current executable image located in the
-- remote home directory at the same place as here. If the image does not exist
-- it can be installed, if it exists it can be updated if the sha256 does not
-- match with the sha256 of local image. Image can be named with a platform
-- suffix so that we can have cross platform access.

-- | Note we can use rpc to local host as well for executing a function as some
-- other user on the same host without using sudo.
defaultConfig :: Executable -> RunningConfig
defaultConfig exe =
    RunningConfig
    { rcRemote = Nothing
    , rcUser = Nothing
    , rcSudo = False
    , rcImage = exe
    , rcImageFlags = ""
    , rcLogger = const (pure ())
    }

-- We've changed the name of this from "using" to "exec". haskell-src-exts
-- fails to parse the keword "using". Did not explore this in depth.

-- | The RPC executable to use on the server for making the RPC call.
--
-- The executable is invoked as @image <image flags> endpoint@.
setImage :: Executable -> RunningConfig -> RunningConfig
setImage x rc = rc { rcImage = x }

-- | Use these flags when invoking the RPC server image.
--
-- Default is "" i.e. no flags.
setImageFlags :: String -> RunningConfig -> RunningConfig
setImageFlags x rc = rc { rcImageFlags = x }

-- | Modify the server name to use for ssh access.
--
-- Default is local host IP address "127.0.0.1".
setRemote :: (String, String) -> RunningConfig -> RunningConfig
setRemote x rc = rc { rcRemote = Just x }

-- | Modify the ssh username to use for running the endpoint.
--
-- Default is the current user.
setUser :: String -> RunningConfig -> RunningConfig
setUser x rc = rc { rcUser = Just x }

-- | Use root privileges for running the endpoint.
--
-- Default is 'False'.
useSudo :: Bool -> RunningConfig -> RunningConfig
useSudo x rc = rc { rcSudo = x }

-- | Use the supplied logger to for log and trace.
--
-- Default is 'putStrLn'.
setLogger :: (String -> IO ()) -> RunningConfig -> RunningConfig
setLogger x rc = rc { rcLogger = x }

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
-- XXX we should directly use "with" functionality in "call"? We do not need
-- this to be generic it could be ssh specific.

-- NOTE: We could define the client side and server side functions separately
-- and that will allow the client side call to be simpler as we do not need an
-- indirection to access the call field of the record, but that complicates the
-- exports and we will have to export both the symbols separately in the export
-- list. We can possibly write a macro to export all three at the same time,
-- local, client, server.

-- | Represents an RPC endpoint consisting of:
--
-- * @run@ is the same as the original function
-- * @call@ takes a 'Runner' and converts it into a local function call. The
-- arguments from the local call are converted to IR and supplied to the
-- Runner, similarly the result from the Runner is converted to Haskell type
-- and returned from the local call.  Used at the client side invocation of the
-- function.
-- * @evaluator@ is a pair of rpc symbol name and a wrapper over the original
-- function which takes IR arguments and returns an IR result. The wrapper
-- converts the IR arguments to Haskell types and converts the Haskell result
-- type of the original function to IR.
data RpcSymbol ir typ =
    RpcSymbol
        { run :: typ                  -- original function for local call
        , call :: Runner ir -> typ    -- client side generic (using IR) call
        , evaluator :: RpcEval ir     -- server side generic (using IR) call
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
