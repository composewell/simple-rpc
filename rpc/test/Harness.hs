-- | A minimal test harness: named test cases, assertions and QuickCheck
-- properties with a configurable seed.
--
module Harness
    ( Test
    , test
    , expectFailure
    , runTests
    , assertEqual
    , assertBool
    , assertThrows
    , property
    ) where

-------------------------------------------------------------------------------
-- Imports
-------------------------------------------------------------------------------

import Control.Exception
    ( Exception, SomeException(..), displayException, evaluate, throwIO
    , try )
import Control.Monad (unless, when)
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import System.Exit (exitFailure)
import System.IO (hFlush, stdout)
import System.IO.Unsafe (unsafePerformIO)
import System.Timeout (timeout)
import Test.QuickCheck (Args(..), Result, Testable, isSuccess, stdArgs)
import Test.QuickCheck.Random (mkQCGen)

import qualified Test.QuickCheck as QC

-------------------------------------------------------------------------------
-- Assertions
-------------------------------------------------------------------------------

newtype TestFailure = TestFailure String deriving Show

instance Exception TestFailure where
    displayException (TestFailure msg) = msg

-- | Raise a 'TestFailure' exception with the message if the condition is
-- false.
assertBool :: String -> Bool -> IO ()
assertBool msg cond = unless cond $ throwIO (TestFailure msg)

-- | @assertEqual msg expected actual@.
assertEqual :: (Eq a, Show a) => String -> a -> a -> IO ()
assertEqual msg expected actual =
    unless (expected == actual)
        $ throwIO
        $ TestFailure
        $ msg ++ "\n  expected: " ++ show expected
              ++ "\n  actual:   " ++ show actual

-- | @assertThrows msg p action@ runs @action@, forces its result to WHNF and
-- checks that it raises an exception of type @e@ satisfying @p@.
assertThrows :: (Exception e, Show a) => String -> (e -> Bool) -> IO a -> IO ()
assertThrows msg p action = do
    r <- try (action >>= evaluate)
    case r of
        Left e ->
            unless (p e)
                $ throwIO
                $ TestFailure
                $ msg ++ "\n  unexpected exception: " ++ show e
        Right v ->
            throwIO
                $ TestFailure
                $ msg ++ "\n  no exception, result: " ++ show v

-------------------------------------------------------------------------------
-- QuickCheck
-------------------------------------------------------------------------------

{-# NOINLINE seedRef #-}
seedRef :: IORef Int
seedRef = unsafePerformIO (newIORef 0)

-- | Check a QuickCheck property with the seed given to 'runTests', raise a
-- 'TestFailure' exception if it fails.
property :: Testable p => p -> IO ()
property p = do
    seed <- readIORef seedRef
    let args =
            stdArgs
                { replay = Just (mkQCGen seed, 0)
                , chatty = False
                , maxSuccess = 100
                }
    r :: Result <- QC.quickCheckWithResult args p
    unless (isSuccess r) $ throwIO $ TestFailure (QC.output r)

-------------------------------------------------------------------------------
-- Running
-------------------------------------------------------------------------------

data Test
    = Test String (IO ())
    | ExpectFailure String String (IO ())

-- | A named test case.
test :: String -> IO () -> Test
test = Test

-- | @expectFailure reason name action@ is a test case that fails because of
-- a known defect described by @reason@. It is reported as XFAIL when it
-- fails, and fails as XPASS when it passes, the defect is then fixed.
expectFailure :: String -> String -> IO () -> Test
expectFailure reason name = ExpectFailure name reason

-- | Seconds after which a test is reported as failed.
testTimeout :: Int
testTimeout = 120

-- | Run the tests with the QuickCheck seed, print the result of each test and
-- exit with a failure exit code if any test failed.
runTests :: Int -> [Test] -> IO ()
runTests seed tests = do
    writeIORef seedRef seed
    putStrLn $ "QuickCheck seed: " ++ show seed
    results <- mapM runOne tests
    let failed = [name | (name, False) <- results]
    putStrLn ""
    putStrLn
        $ show (length tests - length failed) ++ " passed, "
            ++ show (length failed) ++ " failed"
    mapM_ (putStrLn . ("FAILED: " ++)) failed
    when (not (null failed)) exitFailure

    where

    run name action = do
        putStr (name ++ " ... ")
        hFlush stdout
        r <- try (timeout (testTimeout * 1000000) action)
        pure $ case r of
            Right (Just ()) -> Nothing
            Right Nothing ->
                Just $ "timed out after " ++ show testTimeout ++ " seconds"
            -- displayException of SomeException adds the backtrace
            Left (SomeException e) -> Just (displayException e)

    runOne (Test name action) = do
        r <- run name action
        case r of
            Nothing -> putStrLn "OK" >> pure (name, True)
            Just err -> do
                putStrLn "FAIL"
                putStrLn $ "  " ++ err
                pure (name, False)

    runOne (ExpectFailure name reason action) = do
        r <- run name action
        case r of
            Nothing -> do
                putStrLn "XPASS"
                putStrLn $ "  expected to fail: " ++ reason
                pure (name, False)
            Just err -> do
                putStrLn "XFAIL"
                putStrLn $ "  " ++ reason
                putStrLn $ "  " ++ err
                pure (name, True)
