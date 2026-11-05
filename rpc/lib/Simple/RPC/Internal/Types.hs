{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE QuantifiedConstraints #-}

-- | Types and functions common to client and server.
--
module Simple.RPC.Internal.Types
    ( -- * Intermediate Representation (IR)
      IntermediateRep
    , toIntermediateRep
    , resultFromIR

    -- ** IR Arrays
    , createArr
    , irArgs
    , irArg

    -- ** IR Maps
    , createObj
    , objLookup

    -- * IR Serialization
    , toBinStream
    , toChunkStream
    , fromBinStream
    , irFromArray
    , irToString
    , decodeUtf8Array

    -- * Symbols
    , RpcEval(..)
    , RpcMap
    , RpcSymbol(..)
    , rpcName
    , rpcServiceFunction
    , rpcCallUsing
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

    -- * Exceptions
    , RpcException(..)
    ) where

-------------------------------------------------------------------------------
-- Imports
-------------------------------------------------------------------------------

import Control.Exception (Exception(..), throwIO)
import Data.Aeson (ToJSON, FromJSON, toJSON, fromJSON)
import Data.Functor.Identity (runIdentity)
import Data.Vector ((!?))
import Data.Word (Word8)
import Streamly.Data.Array (Array)
import Streamly.Data.Stream (Stream)

import qualified Data.Aeson as Json
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KeyMap
import qualified Data.Bifunctor
import qualified Data.HashMap.Strict as HM
import qualified Data.Vector as Vector
import qualified Streamly.Data.Array as Array
import qualified Streamly.Data.Stream as Stream
import qualified Streamly.External.ByteString as StreamlyBS
import qualified Streamly.External.ByteString.Lazy as StreamlyBSL
import qualified Streamly.Unicode.Stream as Unicode

-------------------------------------------------------------------------------
-- Exceptions
-------------------------------------------------------------------------------

-- | Exceptions raised by the RPC client and the RPC server.
data RpcException
    = EndpointNotFound String
      -- ^ The server does not have an endpoint with this name.
    | InvalidInput String
      -- ^ The server could not decode the input of the endpoint, the
      -- argument is the error message.
    | InvalidOutput String
      -- ^ The client could not decode the last output line of the server,
      -- the argument is the error message.
    | EmptyOutput
      -- ^ The server did not print any output line.
    | VersionMismatch String String String
      -- ^ @VersionMismatch executable expected actual@: the server
      -- executable printed the version @actual@ instead of @expected@.
    | InvalidUsage [String]
      -- ^ The server executable was called with these arguments, it accepts
      -- at most one argument.
    deriving (Eq, Show)

instance Exception RpcException where
    displayException e =
        case e of
            EndpointNotFound name -> "RPC endpoint not found: " ++ name
            InvalidInput msg -> "Invalid RPC input: " ++ msg
            InvalidOutput msg -> "Invalid RPC output: " ++ msg
            EmptyOutput -> "The RPC server did not print an output line"
            VersionMismatch exe expected actual ->
                "Version mismatch for " ++ exe
                    ++ ", expected: " ++ show expected
                    ++ ", got: " ++ show actual
            InvalidUsage args ->
                "Usage: <executable> [<endpoint>], got the arguments: "
                    ++ show args

-------------------------------------------------------------------------------
-- Running Config
-------------------------------------------------------------------------------

-- | RPC server executable path and version.
--
-- Before each call, the client runs the executable without arguments and
-- compares its stdout with 'executableVersion'. The executable must print
-- exactly this string, 'Simple.RPC.Server.mainWith' prints it without a
-- trailing newline.
data Executable =
    Executable
        { executablePath :: FilePath
        , executableVersion :: String
        }

-- XXX CallSpec
-- XXX Currently user is specified at two places user@hostname and rcUser. We
-- need to have a single mechanism.

-- | RPC server specification: server, port, username, executable location,
-- privileges to use etc. See 'defaultConfig' and the setters for the meaning
-- of the fields.
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

-- | A config that runs the executable on the local host as the current user,
-- without flags, and logs with 'putStrLn':
--
-- * 'rcRemote' is 'Nothing', the executable is run without ssh,
-- * 'rcUser' is 'Nothing',
-- * 'rcSudo' is 'False',
-- * 'rcImage' is the argument,
-- * 'rcImageFlags' is @""@,
-- * 'rcLogger' is 'putStrLn'.
defaultConfig :: Executable -> RunningConfig
defaultConfig exe =
    RunningConfig
    { rcRemote = Nothing
    , rcUser = Nothing
    , rcSudo = False
    , rcImage = exe
    , rcImageFlags = ""
    -- , rcLogger = const (pure ())
    , rcLogger = putStrLn
    }

-- We've changed the name of this from "using" to "exec". haskell-src-exts
-- fails to parse the keword "using". Did not explore this in depth.

-- | The RPC executable to use on the server for making the RPC call.
--
-- The executable is invoked as @image <image flags> endpoint@.
setImage :: Executable -> RunningConfig -> RunningConfig
setImage x rc = rc { rcImage = x }

-- | Use these flags when invoking the RPC server image for an endpoint call.
-- The flags are split into words at white space, a word cannot contain white
-- space or quotes. The version check invokes the image without the flags.
--
-- The server accepts one argument, the endpoint name, therefore the flags
-- can only be options that the server program does not see, e.g. GHC RTS
-- options @+RTS -N -RTS@ if the server is linked with @-rtsopts@.
--
-- Default is "" i.e. no flags.
setImageFlags :: String -> RunningConfig -> RunningConfig
setImageFlags x rc = rc { rcImageFlags = x }

-- | @setRemote (address, port)@ sets the config to run the executable on a
-- remote host. An endpoint call then runs
-- @ssh -T -p port -o "StrictHostKeyChecking off" address command@. The
-- address is passed to ssh as it is, e.g. @user\@host@.
--
-- The remote command is run by the shell of the ssh user, the client quotes
-- it for a POSIX shell. The remote host must have a POSIX shell as the
-- shell of the ssh user, a Windows host is not supported. The client can be
-- on Windows, it runs @ssh.exe@.
--
-- Default is 'Nothing', the executable is run on the local host without ssh.
--
-- /Dependencies/: @ssh@ on the local host, a POSIX shell for the ssh user on
-- the remote host.
setRemote :: (String, String) -> RunningConfig -> RunningConfig
setRemote x rc = rc { rcRemote = Just x }

-- | Set the config to run the endpoint as this user. An endpoint call then
-- runs @sudo su user -c command@. The current user, or the ssh user if
-- 'setRemote' is used, must be allowed to run @sudo su@. @su@ runs the
-- command with the shell of the user, the client quotes it for a POSIX
-- shell. The version check is run as the current user.
--
-- Without 'setRemote', @sudo@ and @su@ are run on the local host, which is
-- not supported on Windows.
--
-- Default is 'Nothing', the endpoint is run as the current user.
--
-- /Dependencies/: @sudo@ and @su@ on the host that runs the executable, a
-- POSIX shell for the user.
setUser :: String -> RunningConfig -> RunningConfig
setUser x rc = rc { rcUser = Just x }

-- | Set the config to run the endpoint with @sudo@. An endpoint call then
-- runs @sudo executable ...@. When 'setUser' is also used, @sudo
-- executable ...@ is the command run by @su@, the user must be allowed to
-- run @sudo@.
--
-- Without 'setRemote', @sudo@ is run on the local host, which is not
-- supported on Windows.
--
-- Default is 'False'.
--
-- /Dependencies/: @sudo@ on the host that runs the executable.
useSudo :: Bool -> RunningConfig -> RunningConfig
useSudo x rc = rc { rcSudo = x }

-- | Use the supplied logger for log and trace. The client logs the commands,
-- the input, each output line of the server and each stderr line of the
-- server.
--
-- Default is 'putStrLn'.
setLogger :: (String -> IO ()) -> RunningConfig -> RunningConfig
setLogger x rc = rc { rcLogger = x }

-------------------------------------------------------------------------------
-- Symbols
-------------------------------------------------------------------------------

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
-- * @direct@ is the same as the original function
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
        { direct :: typ               -- original function for local call
        , call :: Runner ir -> typ    -- client side generic (using IR) call
        , evaluator :: RpcEval ir     -- server side generic (using IR) call
        }

-- | Get the name of the RPC endpoint, @Module.name@, where @Module@ is the
-- module that has the 'Simple.RPC.Common.rpcExport' splice and @name@ is the
-- name given to 'Simple.RPC.Common.rpcExport'.
rpcName :: RpcSymbol ir typ -> String
rpcName = symbol . evaluator

-- | Get the server side wrapper of the function to be called. Arguments of the
-- function are in IR and result is also in IR.
rpcServiceFunction :: RpcSymbol ir typ -> ir -> IO ir
rpcServiceFunction = eval . evaluator

-- | A client side wrapper for making a call to the function using a Runner
-- which actually transfers the arguments to remote end, makes the call and
-- returns the result as IR. The job of this funciton is to convert the Haskell
-- type arguments to IR and call the Runner function, and then convert the
-- result of Runner back to Haskell type.
rpcCallUsing :: RpcSymbol ir typ -> Runner ir -> typ
rpcCallUsing = call

-- XXX Map instead of HashMap?
-- XXX rename to ServiceMap

-- | A map of endpoint names and the corresponding server side wrappers.
type RpcMap ir = HM.HashMap String (ir -> IO ir)

-- XXX serviceMapFromList

-- | Create a map from a list of server side wrappers, use 'evaluator' to get
-- the wrapper of an t'RpcSymbol'. If two wrappers have the same endpoint name,
-- the map has the one later in the list.
createRpcMap :: [RpcEval ir] -> RpcMap ir
createRpcMap = HM.fromList . map (\x -> (symbol x, eval x))

-- XXX lookupService

-- | Lookup a symbol in a map.
lookupRpcSymbol :: String -> RpcMap ir -> Maybe (ir -> IO ir)
lookupRpcSymbol = HM.lookup

-------------------------------------------------------------------------------
-- Pretty printing
-------------------------------------------------------------------------------

-- | Format a tag and its value for pretty printing.
--
-- >>> showPair "CMD" "ls -l"
-- "[CMD] ls -l"
showPair :: String -> String -> String
showPair tag val = "[" ++ tag ++ "] " ++ val

-------------------------------------------------------------------------------
-- Serialized values
-------------------------------------------------------------------------------

-- | The intermediate representation of the arguments and the results, a JSON
-- value.
type IntermediateRep = Json.Value

-------------------------------------------------------------------------------
-- Conversions of IR
-------------------------------------------------------------------------------

-- | Convert the Haskell representation of IR (i.e. Json) to a byte stream of
-- its JSON encoding.
--
-- The stream is generated from the chunks of the lazy encoding, the time is
-- linear in the size of the encoding.
{-# INLINE toBinStream #-}
toBinStream :: Monad m => IntermediateRep -> Stream m Word8
toBinStream input =
    Stream.unfold StreamlyBSL.reader (Json.encode input)

-- | Convert the Haskell representation of IR (i.e. Json) to a stream of
-- arrays of its JSON encoding, one array for each chunk of the lazy encoding.
toChunkStream :: IntermediateRep -> Stream IO (Array Word8)
toChunkStream input =
    StreamlyBSL.toChunks (Json.encode input)

-- | Convert a byte stream of IR (i.e. Json) to its Haskell representation.
fromBinStream :: Stream IO Word8 -> IO IntermediateRep
fromBinStream input = do
    res <- Stream.fold StreamlyBS.write input
    case Json.decodeStrict res of
        Nothing -> error "Unable to decode"
        Just x -> pure x

-- | Decode a UTF-8 encoded byte array to a String, replacing invalid bytes
-- with the Unicode replacement character. Used for logging.
decodeUtf8Array :: Array Word8 -> String
decodeUtf8Array arr =
    runIdentity $ Stream.toList $ Unicode.decodeUtf8 $ Array.read arr

-- | Decode the UTF-8 encoded JSON representation of IR in a byte array.
-- Returns the decoding error message on failure.
irFromArray :: Array Word8 -> IO (Either String IntermediateRep)
irFromArray arr =
    pure $ Json.eitherDecodeStrict' (StreamlyBS.fromArray arr)

-- | Convert the Haskell representation of IR (i.e. Json) to String. Used for
-- logging.
irToString :: IntermediateRep -> String
irToString input =
    runIdentity $ Stream.toList $ Unicode.decodeUtf8 $ toBinStream input

-------------------------------------------------------------------------------
-- Conerting Haskell values to/from IR
-------------------------------------------------------------------------------

-- | Convert a Haskell value to IR, the same as 'toJSON'.
--
-- >>> toIntermediateRep (1 :: Int, "a")
-- Array [Number 1.0,String "a"]
toIntermediateRep :: ToJSON a => a -> IntermediateRep
toIntermediateRep = toJSON

-- | Decode the result of an endpoint from IR. Raise an 'InvalidOutput'
-- exception if the IR cannot be decoded to the required type.
resultFromIR :: FromJSON a => IntermediateRep -> IO a
resultFromIR val =
    case fromJSON val of
        Json.Error err -> throwIO $ InvalidOutput err
        Json.Success a -> pure a

-------------------------------------------------------------------------------
-- Arrays in IR
-------------------------------------------------------------------------------

-- | @irArgs n ir@ returns the elements of the IR array @ir@. Raise an
-- 'InvalidInput' exception if @ir@ is not an array of @n@ elements.
irArgs :: Int -> IntermediateRep -> IO (Vector.Vector IntermediateRep)
irArgs n (Json.Array vec)
    | Vector.length vec == n = pure vec
irArgs n ir =
    throwIO
        $ InvalidInput
        $ "expected an array of " ++ show n ++ " arguments, got: "
            ++ irToString ir

-- | @irArg args i@ decodes the element at index @i@ of the arguments returned
-- by 'irArgs'. Raise an 'InvalidInput' exception if the index is out of
-- bounds or the element cannot be decoded to the required type.
irArg :: FromJSON a => Vector.Vector IntermediateRep -> Int -> IO a
irArg args i =
    case args !? i of
        Nothing -> throwIO $ InvalidInput $ "missing argument " ++ show i
        Just v ->
            case fromJSON v of
                Json.Error err ->
                    throwIO
                        $ InvalidInput
                        $ "argument " ++ show i ++ ": " ++ err
                Json.Success a -> pure a

-- | Create an array in IR from a list of IR Haskell values.
createArr :: [IntermediateRep] -> IntermediateRep
createArr = Json.Array . Vector.fromList

-------------------------------------------------------------------------------
-- Key-value maps in IR
-------------------------------------------------------------------------------

-- XXX swap the arguments?

-- | Lookup a value corresponding to a key in the IR representation of a
-- key-value map.
objLookup :: FromJSON a => IntermediateRep -> String -> a
objLookup (Json.Object obj) kStr =
    case KeyMap.lookup (Key.fromString kStr) obj of
        Just val ->
            case fromJSON val of
                Json.Error err -> error err
                Json.Success a -> a
        Nothing -> error $ "Could not find: " ++ kStr
objLookup _ _ = error "Value is not an object type"

-- | Convert (String, IR) key-value pairs to a map in IR.
createObj :: [(String, IntermediateRep)] -> IntermediateRep
createObj kv = Json.object $ map (Data.Bifunctor.first Key.fromString) kv
