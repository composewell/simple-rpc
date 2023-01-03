{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE QuantifiedConstraints #-}

module Simple.RPC.Types
    ( -- Intermediate
      IntermediateRep
    , toIntermediateRep
    , fromIntermediateRep
    , objLookup
    , arrIndex
    , ensureNullArray
    , createObj
    , createArr
    , toBinStream
    , fromBinStream
    , irFromString

      -- Rpc
    , Runner
    , RpcEval(..)
    , RpcSymbol(..)

      -- Extended
    , Executable(..)
    , Username
    , SSHConfig
    , RunningConfig(..)
    , exec
    , onSSH
    , asUser
    ) where

--------------------------------------------------------------------------------
-- Imports
--------------------------------------------------------------------------------

import Data.Function ((&))
import Data.Word (Word8)
import Streamly.Data.Stream (Stream)
import Data.Vector ((!?))

import qualified Data.Vector as Vector
import qualified Data.Aeson.KeyMap as KeyMap
import qualified Data.Aeson.Key as Key
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString.Lazy.Char8 as BSLC
import qualified Streamly.Data.Stream as Stream

import Data.Aeson

--------------------------------------------------------------------------------
-- Main types
--------------------------------------------------------------------------------

type IntermediateRep = Value

type Runner ir = String -> ir -> IO ir

data RpcEval ir =
    RpcEval
        { symbol :: String
        , eval :: ir -> IO ir
        }

data RpcSymbol ir typ =
    RpcSymbol
        { run :: typ                                -- raw
        , call :: Runner ir -> typ                  -- client
        , evaluator :: RpcEval ir                   -- server
        }

--------------------------------------------------------------------------------
-- Extended types
--------------------------------------------------------------------------------

data RunningConfig =
    RunningConfig
        { rcSSH :: Maybe SSHConfig
        , rcUser :: Maybe Username
        , rcExe :: Executable
        }

type Username = String

type SSHConfig = (String, String)

data Executable =
    Executable
        { executablePath :: FilePath
        , executableVersion :: String
        }

-- We've changed this from using to exec. haskell-src-exts fails to parse the
-- keword "using". Did not explore this in depth.
exec :: Executable -> RunningConfig
exec x = RunningConfig Nothing Nothing x

onSSH :: SSHConfig -> RunningConfig -> RunningConfig
onSSH x rc = rc { rcSSH = Just x }

asUser :: Username -> RunningConfig -> RunningConfig
asUser x rc = rc { rcUser = Just x }

--------------------------------------------------------------------------------
-- Utils
--------------------------------------------------------------------------------

toIntermediateRep :: ToJSON a => a -> IntermediateRep
toIntermediateRep = toJSON

fromIntermediateRep :: FromJSON a => IntermediateRep -> a
fromIntermediateRep val =
    case fromJSON val of
        Error err -> error err
        Success a -> a

ensureNullArray :: IntermediateRep -> a -> a
ensureNullArray (Array vec) act =
    if Vector.length vec == 0
    then act
    else error "Input is not an empty"
ensureNullArray _ _ = error "Value is not an array type"

arrIndex :: FromJSON a => IntermediateRep -> Int -> a
arrIndex (Array vec) i =
    case vec !? i of
        Nothing -> error $ "Could not find arg at position: " ++ show i
        Just val -> fromIntermediateRep val
arrIndex _ _ = error "Value is not an array type"

createArr :: [IntermediateRep] -> IntermediateRep
createArr = Array . Vector.fromList

objLookup :: FromJSON a => IntermediateRep -> String -> a
objLookup (Object obj) kStr =
    case KeyMap.lookup (Key.fromString kStr) obj of
        Just val -> fromIntermediateRep val
        Nothing -> error $ "Could not find: " ++ kStr
objLookup _ _ = error "Value is not an object type"

createObj :: [(String, IntermediateRep)] -> IntermediateRep
createObj kv = object $ map (\(k, v) -> (Key.fromString k, v)) kv

toBinStream :: IntermediateRep -> Stream IO Word8
toBinStream input =
    Stream.enumerateFromTo 0 (len - 1)
        & fmap (BSL.index bsl)

    where
    bsl = encode input
    len = BSL.length bsl

fromBinStream :: Stream IO Word8 -> IO IntermediateRep
fromBinStream input = do
    res <- Stream.toList input
    case decode $ BSL.pack res of
        Nothing -> error "Unable to decode"
        Just x -> pure x

irFromString :: String -> IntermediateRep
irFromString input = do
    case decode $ BSLC.pack input of
        Nothing -> error $ "Unable to decode: " ++ input
        Just x -> x
