{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

-------------------------------------------------------------------------------
-- Imports
-------------------------------------------------------------------------------

import Control.Exception (SomeException, finally)
import Data.Function ((&))
import Data.Functor.Identity (runIdentity)
import Data.IORef (modifyIORef, newIORef, readIORef)
import Data.List (isPrefixOf, sort)
import Streamly.Internal.System.Process (ProcessFailure(..))
import System.Directory (doesFileExist, executable, getPermissions)
import System.Environment (getArgs, lookupEnv)
import System.Exit (ExitCode(..))
import System.FilePath ((</>))
import Test.QuickCheck (forAll, ioProperty, withMaxSuccess)

import qualified Data.Aeson as Json
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import qualified Data.ByteString.Lazy as BSL
import qualified Data.HashMap.Strict as HM
import qualified Streamly.Data.Stream as Stream
import qualified Test.QuickCheck as QC

import Harness
import Simple.RPC
import TestFunctions
import TestUtil

-------------------------------------------------------------------------------
-- Helpers
-------------------------------------------------------------------------------

-- | A config for the test server that does not log.
serverConfig :: Env -> RunningConfig
serverConfig env =
    defaultConfig (Executable (envExe env) testVersion)
        & setLogger (const (pure ()))

-- | A config for the test server that logs to the returned action.
loggingConfig :: Env -> IO (RunningConfig, IO [String])
loggingConfig env = do
    ref <- newIORef []
    let cfg = serverConfig env & setLogger (\s -> modifyIORef ref (s:))
    pure (cfg, reverse <$> readIORef ref)

-- | Print-able ASCII characters and the control characters.
asciiString :: QC.Gen String
asciiString = QC.listOf (QC.choose ('\0', '\127'))

-------------------------------------------------------------------------------
-- Simple.RPC.Common
-------------------------------------------------------------------------------

commonTests :: [Test]
commonTests =
    [ test "toIntermediateRep is toJSON" $ do
        property $ \(x :: Int) -> toIntermediateRep x == Json.toJSON x
        property $ \(x :: String) -> toIntermediateRep x == Json.toJSON x
        property $ \(x :: [(Bool, Maybe Int)]) ->
            toIntermediateRep x == Json.toJSON x

    , test "toBinStream is the JSON encoding" $
        property $ forAll genValue $ \v ->
            let bytes = runIdentity (Stream.toList (toBinStream v))
             in bytes == BSL.unpack (Json.encode v)
                    && Json.decode (BSL.pack bytes) == Just v

    , test "rpcName is the module qualified endpoint name" $ do
        assertEqual "echoInt" "TestFunctions.echoInt" (rpcName echoInt)
        assertEqual "constant" "TestFunctions.constant" (rpcName constant)

    , test "rpcExport run calls the original function" $ do
        r1 <- run add 2 3
        assertEqual "add" 5 r1
        r2 <- run constant
        assertEqual "constant" 42 r2
        r3 <- run three 1 "x" True
        assertEqual "three" (1, "x", True) r3
    ]

-------------------------------------------------------------------------------
-- Simple.RPC.Server
-------------------------------------------------------------------------------

serverTests :: Env -> [Test]
serverTests env =
    [ test "createRpcMap maps endpoint names" $
        assertEqual "keys"
            (sort
                [ "TestFunctions.add", "TestFunctions.constant"
                , "TestFunctions.echoInt", "TestFunctions.echoString"
                , "TestFunctions.failing", "TestFunctions.printPartial"
                , "TestFunctions.replicateChar", "TestFunctions.three"
                , "TestFunctions.writeMarker"
                ])
            (sort (HM.keys rpcMap))

    , test "mainWith without arguments prints the version" $ do
        (code, out, _) <- runServerRaw env [] [] ""
        assertEqual "exit code" ExitSuccess code
        assertEqual "stdout" (BSC.pack testVersion) out

    , test "mainWith prints a non-ASCII version in the C locale" $ do
        let vars =
                [ ("LC_ALL", "C"), ("LANG", "C")
                , ("SIMPLE_RPC_TEST_VERSION", "unicode")
                ]
        (code, out, err) <- runServerRaw env vars [] ""
        assertEqual ("exit code, stderr: " ++ show err) ExitSuccess code
        assertEqual "stdout" (utf8 "1.0-\233") out

    , test "mainWith with two arguments fails" $ do
        (code, _, _) <- runServerRaw env [] ["a", "b"] ""
        assertEqual "exit code" (ExitFailure 1) code

    , test "invokeFunction output protocol" $ do
        (code, out, _) <-
            runServerRaw env [] ["TestFunctions.add"] "[2,3]"
        assertEqual "exit code" ExitSuccess code
        assertEqual "stdout"
            (BSC.pack "[SERVER] TestFunctions.add [2,3]\n\n5") out

    , test "invokeFunction prints the endpoint output before the result" $ do
        (code, out, err) <-
            runServerRaw env [] ["TestFunctions.printPartial"] "[\"abc\"]"
        assertEqual "exit code" ExitSuccess code
        assertEqual "stdout"
            (BSC.pack
                "[SERVER] TestFunctions.printPartial [\"abc\"]\nabc\n\"abc\"")
            out
        assertEqual "stderr" (BSC.pack "printPartial on stderr\n") err

    , test "invokeFunction reads the input up to the first newline" $ do
        (code, out, _) <-
            runServerRaw env [] ["TestFunctions.add"] "[2,3]\ngarbage"
        assertEqual "exit code" ExitSuccess code
        assertEqual "last line" "5" (last (BSC.lines out))

    , test "invokeFunction with a zero argument endpoint" $ do
        (code, out, _) <-
            runServerRaw env [] ["TestFunctions.constant"] "[]"
        assertEqual "exit code" ExitSuccess code
        assertEqual "last line" "42" (last (BSC.lines out))
        (code1, _, _) <-
            runServerRaw env [] ["TestFunctions.constant"] "[1]"
        assertEqual "non-empty input" (ExitFailure 1) code1

    , test "invokeFunction with an unknown endpoint fails" $ do
        (code, _, err) <- runServerRaw env [] ["TestFunctions.nope"] "[]"
        assertEqual "exit code" (ExitFailure 1) code
        assertBool "stderr names the endpoint"
            (BSC.pack "TestFunctions.nope" `BS.isInfixOf` err)

    , test "invokeFunction with an argument of a wrong type fails" $ do
        (code, _, _) <-
            runServerRaw env [] ["TestFunctions.add"] "[2,\"x\"]"
        assertEqual "exit code" (ExitFailure 1) code
    ]

-------------------------------------------------------------------------------
-- Simple.RPC.Client
-------------------------------------------------------------------------------

configTests :: [Test]
configTests =
    [ test "defaultConfig fields" $ do
        let exe = Executable "/x/server" "1.2"
            cfg = defaultConfig exe
        assertEqual "rcRemote" Nothing (rcRemote cfg)
        assertEqual "rcUser" Nothing (rcUser cfg)
        assertEqual "rcSudo" False (rcSudo cfg)
        assertEqual "rcImageFlags" "" (rcImageFlags cfg)
        assertEqual "executablePath" "/x/server"
            (executablePath (rcImage cfg))
        assertEqual "executableVersion" "1.2"
            (executableVersion (rcImage cfg))

    , test "config setters set their fields" $ do
        ref <- newIORef []
        let cfg =
                defaultConfig (Executable "/x/server" "1.2")
                    & setImage (Executable "/y/server" "3")
                    & setImageFlags "+RTS -N -RTS"
                    & setRemote ("u@h", "2222")
                    & setUser "alice"
                    & useSudo True
                    & setLogger (\s -> modifyIORef ref (s:))
        assertEqual "setImage" "/y/server" (executablePath (rcImage cfg))
        assertEqual "setImage version" "3" (executableVersion (rcImage cfg))
        assertEqual "setImageFlags" "+RTS -N -RTS" (rcImageFlags cfg)
        assertEqual "setRemote" (Just ("u@h", "2222")) (rcRemote cfg)
        assertEqual "setUser" (Just "alice") (rcUser cfg)
        assertEqual "useSudo" True (rcSudo cfg)
        assertEqual "useSudo False" False (rcSudo (useSudo False cfg))
        rcLogger cfg "hello"
        logged <- readIORef ref
        assertEqual "setLogger" ["hello"] logged

    , test "showPair formats a tag and a value" $ do
        assertEqual "CMD" "[CMD] ls -l" (showPair "CMD" "ls -l")
        assertEqual "empty" "[] " (showPair "" "")
    ]

clientTests :: Env -> [Test]
clientTests env =
    [ test "runAt calls a local endpoint" $ do
        let cfg = serverConfig env
        r1 <- runAt cfg add 2 3
        assertEqual "add" 5 r1
        r2 <- runAt cfg constant
        assertEqual "constant" 42 r2
        r3 <- runAt cfg three (-7) "x y" False
        assertEqual "three" (-7, "x y", False) r3

    , test "runAt round trips Int and ASCII String arguments" $ do
        let cfg = serverConfig env
        property $ withMaxSuccess 20 $ \(x :: Int) ->
            ioProperty $ (== x) <$> runAt cfg echoInt x
        property $ withMaxSuccess 20 $ forAll asciiString $ \s ->
            ioProperty $ (== s) <$> runAt cfg echoString s

    , test "runAt returns the result after partial endpoint output" $ do
        r <- runAt (serverConfig env) printPartial "no newline"
        assertEqual "printPartial" "no newline" r

    , test "runAt logs the command, input, stdout and stderr" $ do
        (cfg, getLog) <- loggingConfig env
        _ <- runAt cfg printPartial "abc"
        logged <- getLog
        let has p = any p logged
        assertBool ("CMD: " ++ show logged)
            (has ("[CMD] " `isPrefixOf`))
        assertBool ("ACT: " ++ show logged)
            (has (== "[ACT] TestFunctions.printPartial"))
        assertBool ("input: " ++ show logged) (has (== "[\"abc\"]"))
        assertBool ("STDOUT: " ++ show logged)
            (has (== "[STDOUT] \"abc\""))
        assertBool ("STDERR: " ++ show logged)
            (has (== "[STDERR] printPartial on stderr"))

    , test "runAt raises ProcessFailure when the endpoint fails" $
        assertThrows "failing" (\(ProcessFailure n) -> n == 1)
            (runAt (serverConfig env) failing 1)

    , test "runAt raises an exception on a version mismatch" $
        assertThrows "version" (\(_ :: SomeException) -> True)
            (runAt
                (defaultConfig (Executable (envExe env) "other")
                    & setLogger (const (pure ())))
                add 1 2)

    , test "runAt with setUser runs the endpoint through sudo su" $ do
        clearFakeLog env
        r <- runAt (serverConfig env & setUser "rpcuser") add 2 3
        assertEqual "add" 5 r
        logged <- readFakeLog env
        assertBool ("su: " ++ show logged)
            (any ("su [rpcuser] [-c] " `isPrefixOf`) logged)

    , test "runAt with useSudo runs the endpoint through sudo" $ do
        clearFakeLog env
        r <- runAt (serverConfig env & useSudo True) add 2 3
        assertEqual "add" 5 r
        logged <- readFakeLog env
        assertBool ("sudo: " ++ show logged)
            (any ("sudo [" `isPrefixOf`) logged)

    , test "runAt with setRemote runs the endpoint through ssh" $ do
        clearFakeLog env
        r <- runAt (serverConfig env & setRemote ("u@h", "2222")) add 2 3
        assertEqual "add" 5 r
        logged <- readFakeLog env
        let sshPrefix =
                "ssh [-T] [-p] [2222] [-o] [StrictHostKeyChecking off] [u@h] "
        assertBool ("ssh: " ++ show logged)
            (length (filter (sshPrefix `isPrefixOf`) logged) == 2)

    , test "runAt with setRemote and setUser" $ do
        clearFakeLog env
        let cfg =
                serverConfig env
                    & setRemote ("u@h", "22")
                    & setUser "rpcuser"
        r <- runAt cfg three 1 "a" True
        assertEqual "three" (1, "a", True) r
        logged <- readFakeLog env
        assertBool ("su: " ++ show logged)
            (any ("su [rpcuser] [-c] " `isPrefixOf`) logged)
    ]

installTests :: Env -> [Test]
installTests env =
    [ test "installOnRemote copies the executable" $ do
        let destDir = envDir env </> "install1" </> "bin"
            dest = destDir </> "server"
        installOnRemote
            (Executable (envExe env) testVersion) ("u@h", "22") dest
        exists <- doesFileExist dest
        assertBool "destination exists" exists
        perms <- getPermissions dest
        assertBool "destination is executable" (executable perms)
        orig <- BS.readFile (envExe env)
        copy <- BS.readFile dest
        assertBool "same content" (orig == copy)
        logged <- readFakeLog env
        assertBool ("scp: " ++ show logged)
            (any ("scp [-P] [22] " `isPrefixOf`) logged)
    ]

-------------------------------------------------------------------------------
-- Unicode
-------------------------------------------------------------------------------

unicodeTests :: Env -> [Test]
unicodeTests env =
    [ test "invokeFunction decodes UTF-8 input" $ do
        (code, out, err) <-
            runServerRaw env [] ["TestFunctions.echoString"]
                (utf8 "[\"T\233st\8594\"]")
        assertEqual ("exit code, stderr: " ++ show err) ExitSuccess code
        assertEqual "last line"
            (utf8 "\"T\233st\8594\"") (last (BSC.lines out))

    , test "invokeFunction writes UTF-8 output in the C locale" $ do
        let cLocale = [("LC_ALL", "C"), ("LANG", "C")]
        (code, out, err) <-
            runServerRaw env cLocale ["TestFunctions.echoString"]
                (utf8 "[\"\233\"]")
        assertEqual ("echoString, stderr: " ++ show err) ExitSuccess code
        assertEqual "echoString stdout"
            (utf8
                $ "[SERVER] TestFunctions.echoString [\"\233\"]\n"
                    ++ "\n\"\233\"")
            out
        (code1, out1, err1) <-
            runServerRaw env cLocale ["TestFunctions.printPartial"]
                (utf8 "[\"\8594\"]")
        assertEqual ("printPartial, stderr: " ++ show err1) ExitSuccess code1
        assertEqual "printPartial stdout"
            (utf8
                $ "[SERVER] TestFunctions.printPartial [\"\8594\"]\n"
                    ++ "\8594\n\"\8594\"")
            out1

    , test "runAt decodes a UTF-8 result" $ do
        path <- fakeServer env "utf8-server" "1" (utf8 "\"T\233st\8594\"")
        r <-
            runAt
                (serverConfig env & setImage (Executable path "1"))
                echoString "x"
        assertEqual "result" "T\233st\8594" r

    , test "runAt round trips Unicode String arguments" $ do
        let cfg = serverConfig env
        property $ withMaxSuccess 20 $ forAll genUnicodeString $ \s ->
            ioProperty $ (== s) <$> runAt cfg echoString s

    , test "runAt logs the input as UTF-8 decoded text" $ do
        (cfg, getLog) <- loggingConfig env
        _ <- runAt cfg echoString "\233"
        logged <- getLog
        assertBool ("input: " ++ show logged) ("[\"\233\"]" `elem` logged)
    ]

-------------------------------------------------------------------------------
-- Main
-------------------------------------------------------------------------------

-- | Parse @--seed N@ from the arguments, default 0.
parseSeed :: [String] -> Int
parseSeed args =
    case args of
        ("--seed" : n : _) -> read n
        (_ : rest) -> parseSeed rest
        [] -> 0

main :: IO ()
main = do
    mode <- lookupEnv (fst serverMode)
    if mode == Just (snd serverMode)
    then do
        v <- lookupEnv "SIMPLE_RPC_TEST_VERSION"
        let version = if v == Just "unicode" then "1.0-\233" else testVersion
        mainWith version rpcMap
    else do
        seed <- parseSeed <$> getArgs
        env <- setupEnv
        let tests =
                commonTests
                    ++ serverTests env
                    ++ configTests
                    ++ clientTests env
                    ++ installTests env
                    ++ unicodeTests env
        runTests seed tests `finally` cleanupEnv env
