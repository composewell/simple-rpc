{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

-------------------------------------------------------------------------------
-- Imports
-------------------------------------------------------------------------------

import Control.Exception (displayException, evaluate, finally, try)
import Control.Monad (unless)
import Data.Function ((&))
import Data.Functor.Identity (runIdentity)
import Data.Int (Int64)
import Data.IORef (modifyIORef, newIORef, readIORef)
import Data.List (isInfixOf, isPrefixOf, sort)
import GHC.Clock (getMonotonicTime)
import Streamly.Internal.System.Process (ProcessFailure(..))
import System.Directory
    ( copyFile, createDirectory, doesFileExist, executable, exeExtension
    , getPermissions )
import System.Environment (getArgs, lookupEnv)
import System.Exit (ExitCode(..))
import System.FilePath ((<.>), (</>))
import System.IO (hClose, hPutStr)
import System.Info (os)
import System.Process
    ( CreateProcess(std_in, std_out), StdStream(..), callProcess
    , createProcess, proc, readProcess, waitForProcess )
import Test.QuickCheck (forAll, ioProperty, withMaxSuccess)

import qualified Data.Aeson as Json
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import qualified Data.ByteString.Lazy as BSL
import qualified Data.HashMap.Strict as HM
import qualified Streamly.Data.Fold as Fold
import qualified Streamly.Data.Stream as Stream
import qualified Test.QuickCheck as QC

import Fake
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

-- | Check that the stderr of the server has the message of the exception.
assertStderr :: BS.ByteString -> RpcException -> IO ()
assertStderr err e =
    assertBool ("stderr: " ++ show err)
        (utf8 (displayException e) `BS.isInfixOf` err)

-- | Check that the stderr of the server has an 'InvalidInput' message.
assertInvalidInput :: BS.ByteString -> IO ()
assertInvalidInput err =
    assertBool ("stderr: " ++ show err)
        (utf8 "Invalid RPC input: " `BS.isInfixOf` err)

-- | Print-able ASCII characters and the control characters.
asciiString :: QC.Gen String
asciiString = QC.listOf (QC.choose ('\0', '\127'))

isWindows :: Bool
isWindows = os == "mingw32"

-- | The permission bits of a file as shown by @ls -l@, e.g. @rwxr-xr-x@.
-- Uses @sh@ for the same view of the file system as the fake ssh.
lsMode :: FilePath -> IO String
lsMode path = do
    out <- readProcess "sh" ["-c", "ls -ld \"$1\"", "sh", path] ""
    pure $ take 9 $ drop 1 out

-- | Set the mode of a file to an octal mode such as @755@, using @sh@.
chmodSh :: String -> FilePath -> IO ()
chmodSh mode path =
    callProcess "sh" ["-c", "chmod " ++ mode ++ " \"$1\"", "sh", path]

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

    , test "toBinStream time is linear in the size" $ do
        -- About 24 MB of JSON in about 750 chunks. A time quadratic in the
        -- number of chunks takes more than 30 seconds.
        let v = toIntermediateRep [1 .. 3000000 :: Int]
            len = BSL.length (Json.encode v)
        _ <- evaluate len
        t0 <- getMonotonicTime
        -- Force each byte, the stream may have lazy elements
        let count n w = w `seq` n + 1
        total <-
            toBinStream v & Stream.fold (Fold.foldl' count (0 :: Int64))
        t1 <- getMonotonicTime
        assertEqual "length" len total
        assertBool ("took " ++ show (t1 - t0) ++ " seconds") (t1 - t0 < 5)

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
                , "TestFunctions.valRpc", "TestFunctions.writeMarker"
                ])
            (sort (HM.keys rpcMap))

    , test "evaluator in createRpcMap calls the function with IR" $ do
        let m = createRpcMap [evaluator add, evaluator constant]
        case HM.lookup "TestFunctions.add" m of
            Nothing -> assertBool "add not found" False
            Just f -> do
                r <- f (toIntermediateRep [2, 3 :: Int])
                assertEqual "add" (toIntermediateRep (5 :: Int)) r
                assertThrows "add [1]"
                    (\e -> case e of
                        InvalidInput _ -> True
                        _ -> False)
                    (f (toIntermediateRep [1 :: Int]))
        case HM.lookup "TestFunctions.constant" m of
            Nothing -> assertBool "constant not found" False
            Just f -> do
                r <- f (toIntermediateRep ([] :: [Int]))
                assertEqual "constant" (toIntermediateRep (42 :: Int)) r

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

    , test "mainWith with two arguments raises InvalidUsage" $ do
        (code, _, err) <- runServerRaw env [] ["a", "b"] ""
        assertEqual "exit code" (ExitFailure 1) code
        assertStderr err (InvalidUsage ["a", "b"])

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
        (code1, _, err1) <-
            runServerRaw env [] ["TestFunctions.constant"] "[1]"
        assertEqual "non-empty input" (ExitFailure 1) code1
        assertInvalidInput err1

    , test "invokeFunction with an unknown endpoint fails" $ do
        (code, _, err) <- runServerRaw env [] ["TestFunctions.nope"] "[]"
        assertEqual "exit code" (ExitFailure 1) code
        assertStderr err (EndpointNotFound "TestFunctions.nope")

    , test "invokeFunction with invalid JSON input raises InvalidInput" $ do
        (code, _, err) <-
            runServerRaw env [] ["TestFunctions.add"] "not json"
        assertEqual "exit code" (ExitFailure 1) code
        assertInvalidInput err

    , test "invokeFunction with an argument of a wrong type fails" $ do
        (code, _, err) <-
            runServerRaw env [] ["TestFunctions.add"] "[2,\"x\"]"
        assertEqual "exit code" (ExitFailure 1) code
        assertInvalidInput err

    , test "invokeFunction decodes all arguments before the call" $ do
        let marker = envDir env </> "marker"
            input = Json.encode (marker, "not an Int" :: String)
        (code, _, err) <-
            runServerRaw env [] ["TestFunctions.writeMarker"]
                (BSL.toStrict input)
        assertEqual "exit code" (ExitFailure 1) code
        assertInvalidInput err
        exists <- doesFileExist marker
        assertBool "the endpoint was not called" (not exists)

    , test "invokeFunction rejects a wrong number of arguments" $ do
        let call input = do
                (code, _, err) <-
                    runServerRaw env [] ["TestFunctions.add"] input
                assertEqual ("exit code " ++ show input) (ExitFailure 1) code
                assertInvalidInput err
        call "[1,2,3]"
        call "[1]"
        call "[]"
        call "{}"
        call "1"

    , test "rpcExport of a function named val" $ do
        r <- run valRpc 4
        assertEqual "run" 8 r
        (code, out, _) <- runServerRaw env [] ["TestFunctions.valRpc"] "[4]"
        assertEqual "exit code" ExitSuccess code
        assertEqual "last line" "8" (last (BSC.lines out))
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

    , expectFailure
        "streamly-process 0.4.1 writes stdin after the process exits"
        "runAt raises ProcessFailure when the server ignores the input" $ do
        -- The input is larger than the pipe buffer
        path <-
            installFake (envExe env) (envDir env </> "exit3-server")
                (ExitServer "1" 3)
        assertThrows "exit 3" (\(ProcessFailure n) -> n == 3)
            (runAt
                (serverConfig env & setImage (Executable path "1"))
                echoString (replicate 1000000 'x'))

    , test "runAt raises ProcessFailure for an unknown endpoint" $ do
        -- A server that does not have the endpoint, the input is larger
        -- than the pipe buffer.
        path <-
            installFake (envExe env) (envDir env </> "unknown-endpoint-server")
                (ArgsServer (envExe env) ["TestFunctions.nope"])
        (cfg, getLog) <- loggingConfig env
        assertThrows "unknown" (\(ProcessFailure n) -> n == 1)
            (runAt
                (cfg & setImage (Executable path testVersion))
                echoString (replicate 1000000 'x'))
        logged <- getLog
        let msg = displayException (EndpointNotFound "TestFunctions.nope")
            isErr l = "[STDERR]" `isPrefixOf` l
        assertBool ("stderr logged: " ++ show (filter isErr logged))
            (any (\l -> isErr l && msg `isInfixOf` l) logged)

    , test "runAt raises VersionMismatch on a version mismatch" $
        assertThrows "version"
            (== VersionMismatch (envExe env) "other" testVersion)
            (runAt
                (defaultConfig (Executable (envExe env) "other")
                    & setLogger (const (pure ())))
                add 1 2)

    , test "runAt raises EmptyOutput when the server prints nothing" $ do
        path <- fakeServer env "empty-server" "1" ""
        assertThrows "empty" (== EmptyOutput)
            (runAt
                (serverConfig env & setImage (Executable path "1"))
                echoInt 1)

    , test "runAt raises InvalidOutput when the output is not JSON" $ do
        path <- fakeServer env "garbage-server" "1" "garbage"
        assertThrows "garbage"
            (\e -> case e of
                InvalidOutput _ -> True
                _ -> False)
            (runAt
                (serverConfig env & setImage (Executable path "1"))
                echoInt 1)

    , test "runAt decodes the result before it returns" $ do
        path <- fakeServer env "string-server" "1" "\"not an Int\""
        r <-
            try
                $ runAt
                    (serverConfig env & setImage (Executable path "1"))
                    echoInt 1
        case r of
            Left (InvalidOutput _) -> pure ()
            Left e -> assertBool ("unexpected exception: " ++ show e) False
            -- Do not force the result
            Right _ -> assertBool "runAt returned without an exception" False

    , test "runAt passes the setImageFlags flags to the executable" $ do
        let argsFile = envDir env </> "flags-server.args"
        wrapper <-
            installFake (envExe env) (envDir env </> "flags-server")
                (RecordServer (envExe env) argsFile)
        let cfg =
                serverConfig env
                    & setImage (Executable wrapper testVersion)
                    & setImageFlags "+RTS  -A2m\t-RTS "
        r <- runAt cfg add 2 3
        assertEqual "add" 5 r
        calls <- lines <$> readFileFs argsFile
        -- The first call is the version check, without arguments
        assertEqual "arguments"
            [ "call"
            , "call", "+RTS", "-A2m", "-RTS", "TestFunctions.add"
            ]
            calls

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

    , test "installOnRemote keeps the permissions of the directory" $ do
        let destDir = envDir env </> "install2"
            dest = destDir </> "server"
        createDirectory destDir
        chmodSh "755" destDir
        clearFakeLog env
        installOnRemote
            (Executable (envExe env) testVersion) ("u@h", "22") dest
        -- The last scp argument is "u@h:<temporary file>"
        logged <- readFakeLog env
        case filter ("scp " `isPrefixOf`) logged of
            [line] -> do
                let tmp = drop (length ("[u@h:" :: String))
                            $ init $ last $ words line
                tmpExists <- doesFileExist tmp
                assertBool ("temporary file removed: " ++ tmp) (not tmpExists)
            other -> assertBool ("one scp call: " ++ show other) False
        dirMode <- lsMode destDir
        assertEqual "directory mode" "rwxr-xr-x" dirMode
        exeMode <- lsMode dest
        assertEqual "executable mode" "rwxr-xr-x" exeMode

    , test "installOnRemote with special characters in the path" $ do
        let dest = envDir env </> "install3" </> specialDirName </> "srv"
        installOnRemote
            (Executable (envExe env) testVersion) ("u@h", "22") dest
        r <- runAt (serverConfig env & setImage (Executable dest testVersion))
                add 2 3
        assertEqual "installed server" 5 r

    , test "installOnRemote replaces a running executable" $ do
        let dest = envDir env </> "install4" </> "server"
            exe = Executable (envExe env) testVersion
        installOnRemote exe ("u@h", "22") dest
        -- The server waits for its input on stdin
        (Just hin, _, _, ph) <-
            createProcess
                (proc dest ["TestFunctions.add"])
                    { std_in = CreatePipe, std_out = CreatePipe }
        installOnRemote exe ("u@h", "22") dest
            `finally` do
                hPutStr hin "[1,2]"
                hClose hin
                waitForProcess ph
    ]

-------------------------------------------------------------------------------
-- Command quoting
-------------------------------------------------------------------------------

-- | A directory name with characters that a shell interprets.
--
-- If a command is not quoted correctly, the shell expands the variable to
-- an empty string and the glob to the files of the working directory, which
-- 'setupEnv' sets to the temporary directory. Do not use a variable that is
-- set, such as @HOME@: a wrongly quoted @chmod@ on this path once changed
-- the mode of the home directory to 777.
--
-- Windows does not allow the double quote, the backslash and the glob in a
-- file name.
specialDirName :: String
specialDirName =
    if isWindows
    then "dir with 'sq' $SIMPLE_RPC_UNSET `id` \233"
    else "dir with 'sq' \"dq\" $SIMPLE_RPC_UNSET `id` \\ * \233"

quotingTests :: Env -> [Test]
quotingTests env =
    [ test "runAt with special characters in the executable path" $ do
        exe <- specialExe
        r <- runAt (cfgFor exe) add 2 3
        assertEqual "add" 5 r

    , test "runAt with useSudo and special characters in the path" $ do
        exe <- specialExe
        r <- runAt (cfgFor exe & useSudo True) add 2 3
        assertEqual "add" 5 r

    , test "runAt with setUser and special characters in the path" $ do
        exe <- specialExe
        clearFakeLog env
        r <- runAt (cfgFor exe & setUser "rpc 'user'") add 2 3
        assertEqual "add" 5 r
        logged <- readFakeLog env
        assertBool ("su: " ++ show logged)
            (any ("su [rpc 'user'] [-c] " `isPrefixOf`) logged)

    , test "runAt with setRemote and special characters in the path" $ do
        exe <- specialExe
        r <- runAt (cfgFor exe & setRemote ("u@h", "22")) add 2 3
        assertEqual "add" 5 r

    , test "runAt with setRemote, setUser, useSudo and a special path" $ do
        exe <- specialExe
        clearFakeLog env
        let cfg =
                cfgFor exe
                    & setRemote ("u@h", "22")
                    & setUser "rpc 'user'"
                    & useSudo True
        r <- runAt cfg three 1 "a 'b' $c" True
        assertEqual "three" (1, "a 'b' $c", True) r
        logged <- readFakeLog env
        assertBool ("su: " ++ show logged)
            (any ("su [rpc 'user'] [-c] " `isPrefixOf`) logged)
    ]

    where

    cfgFor exe = serverConfig env & setImage (Executable exe testVersion)

    -- A copy of the test executable in specialDirName. A symbolic link
    -- needs extra privileges on Windows.
    specialExe = do
        let dir = envDir env </> specialDirName
            exe = dir </> "server" <.> exeExtension
        exists <- doesFileExist exe
        unless exists $ do
            createDirectory dir
            copyFile (envExe env) exe
        pure exe

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

    , test "runAt logs invalid UTF-8 output with replacement characters" $ do
        path <- fakeServer env "invalid-utf8-server" "1" "\xff\xfe\n7"
        (cfg, getLog) <- loggingConfig env
        r <- runAt (cfg & setImage (Executable path "1")) echoInt 1
        assertEqual "result" 7 r
        logged <- getLog
        assertBool ("log: " ++ show logged)
            ("[STDOUT] \65533\65533" `elem` logged)

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
    fake <- lookupFake
    mode <- lookupEnv (fst serverMode)
    case fake of
        Just f -> runFake f
        Nothing | mode == Just (snd serverMode) -> do
            v <- lookupEnv "SIMPLE_RPC_TEST_VERSION"
            let version =
                    if v == Just "unicode" then "1.0-\233" else testVersion
            mainWith version rpcMap
        Nothing -> runAllTests

    where

    runAllTests = do
        seed <- parseSeed <$> getArgs
        env <- setupEnv
        let tests =
                commonTests
                    ++ serverTests env
                    ++ configTests
                    ++ clientTests env
                    ++ map
                        (skipIf isWindows
                            "the fake remote host is the local host, \
                            \Windows is not a POSIX remote host")
                        (installTests env)
                    ++ unicodeTests env
                    ++ quotingTests env
        runTests seed tests `finally` cleanupEnv env
