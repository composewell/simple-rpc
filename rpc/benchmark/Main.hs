-- | Benchmarks of the IR serialization and of local RPC calls. The benchmark
-- executable runs as the RPC server when the environment variable
-- @SIMPLE_RPC_BENCH_MODE@ is @server@.
--
-- Usage: simple-rpc-bench [ITERATIONS]
--
module Main (main) where

-------------------------------------------------------------------------------
-- Imports
-------------------------------------------------------------------------------

import Control.Exception (evaluate)
import Control.Monad (forM, forM_, void)
import Data.Function ((&))
import GHC.Clock (getMonotonicTime)
import System.Environment (getArgs, getExecutablePath, lookupEnv, setEnv)
import Text.Printf (printf)

import qualified Data.Aeson as Json
import qualified Streamly.Data.Fold as Fold
import qualified Streamly.Data.Stream as Stream

import Simple.RPC

-------------------------------------------------------------------------------
-- Endpoints
-------------------------------------------------------------------------------

benchVersion :: String
benchVersion = "bench-1.0"

raw_replicateChar :: Int -> Char -> IO String
raw_replicateChar n c = pure (replicate n c)
$(rpcExport "raw_replicateChar" "replicateChar")

raw_stringLength :: String -> IO Int
raw_stringLength = pure . length
$(rpcExport "raw_stringLength" "stringLength")

rpcMap :: RpcMap IntermediateRep
rpcMap =
    createRpcMap [evaluator replicateChar, evaluator stringLength]

-------------------------------------------------------------------------------
-- Measurement
-------------------------------------------------------------------------------

-- | @bench iterations name action@ runs the action, prints the minimum and
-- the mean time in milliseconds.
bench :: Int -> String -> IO () -> IO ()
bench iterations name action = do
    times <- forM [1 .. iterations] $ \_ -> do
        t0 <- getMonotonicTime
        action
        t1 <- getMonotonicTime
        pure (t1 - t0)
    let ms x = x * 1000 :: Double
        mean = sum times / fromIntegral (length times)
    printf "%-40s min %9.2f ms  mean %9.2f ms\n"
        name (ms (minimum times)) (ms mean)

-- | A JSON string of @n@ characters.
jsonString :: Int -> IntermediateRep
jsonString n = toIntermediateRep (replicate n 'x')

-------------------------------------------------------------------------------
-- Main
-------------------------------------------------------------------------------

main :: IO ()
main = do
    mode <- lookupEnv "SIMPLE_RPC_BENCH_MODE"
    if mode == Just "server"
    then mainWith benchVersion rpcMap
    else do
        args <- getArgs
        let iterations = case args of
                [n] -> read n
                _ -> 5
        setEnv "SIMPLE_RPC_BENCH_MODE" "server"
        exe <- getExecutablePath
        let cfg =
                defaultConfig (Executable exe benchVersion)
                    & setLogger (const (pure ()))
        forM_ [1, 4] $ \mb -> do
            let n = mb * 1000000
                v = jsonString n
            _ <- evaluate (Json.encode v)
            bench iterations (printf "toBinStream string %d MB" mb)
                $ toBinStream v & Stream.fold Fold.sum >>= void . evaluate
        -- An array of n numbers is about 8n bytes of JSON
        forM_ [125000, 500000] $ \n -> do
            let v = toIntermediateRep [1 .. n :: Int]
                name = printf "toBinStream [Int] %d MB" (n `div` 125000)
            _ <- evaluate (Json.encode v)
            bench iterations name
                $ toBinStream v & Stream.fold Fold.sum >>= void . evaluate
        forM_ [1, 4] $ \mb -> do
            let n = mb * 1000000
                s = replicate n 'x'
            _ <- evaluate (length s)
            bench iterations (printf "runAt input %d MB" mb) $ do
                r <- runAt cfg stringLength s
                _ <- evaluate r
                pure ()
            bench iterations (printf "runAt output %d MB" mb) $ do
                r <- runAt cfg replicateChar n 'x'
                _ <- evaluate (length r)
                pure ()
