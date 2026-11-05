-- | Test environment: a temporary directory, fake @ssh@, @scp@, @sudo@ and
-- @su@ commands, and a runner for the test server process.
--
module TestUtil
    ( Env(..)
    , setupEnv
    , cleanupEnv
    , serverMode
    , readFakeLog
    , clearFakeLog
    , utf8
    , fakeServer
    , runServerRaw
    , genValue
    , genUnicodeString
    ) where

-------------------------------------------------------------------------------
-- Imports
-------------------------------------------------------------------------------

import Control.Concurrent (forkIO)
import Control.Concurrent.MVar (newEmptyMVar, putMVar, takeMVar)
import Control.Exception (IOException, catch)
import Control.Monad (forM_, when)
import Data.Aeson (Value, toJSON)
import System.Directory
    ( createDirectory, findExecutable, getCurrentDirectory
    , getTemporaryDirectory, removeDirectoryRecursive, setCurrentDirectory )
import System.Environment
    (getEnvironment, getExecutablePath, lookupEnv, setEnv, unsetEnv)
import System.Exit (ExitCode)
import System.FilePath (searchPathSeparator, (</>))
import System.IO (hClose, hSetBinaryMode)
import System.Process
    ( CreateProcess(std_err, std_in, std_out), StdStream(..), createProcess
    , getCurrentPid, proc, waitForProcess )
import Test.QuickCheck (Gen)

import qualified Data.Aeson as Json
import qualified Data.Aeson.Key as Key
import qualified Data.ByteString as BS
import qualified Data.ByteString.Builder as BSB
import qualified Data.ByteString.Lazy as BSL
import qualified System.Process as Process
import qualified Test.QuickCheck as QC

import Fake

-------------------------------------------------------------------------------
-- Environment
-------------------------------------------------------------------------------

-- | The name and value of the environment variable that runs the test
-- executable as the RPC server.
serverMode :: (String, String)
serverMode = ("SIMPLE_RPC_TEST_MODE", "server")

data Env =
    Env
        { envDir :: FilePath     -- ^ temporary directory for the tests
        , envOrigDir :: FilePath -- ^ working directory before 'setupEnv'
        , envExe :: FilePath     -- ^ path of the test executable
        , envFakeLog :: FilePath -- ^ invocations of the fake commands
        }

-- | Encode a String as UTF-8.
utf8 :: String -> BS.ByteString
utf8 = BSL.toStrict . BSB.toLazyByteString . BSB.stringUtf8

-- | @fakeServer env name version output@ creates a fake RPC server named
-- @name@ that prints @version@ when called without arguments. Otherwise it
-- reads stdin and prints @output@. Returns the path of the server.
fakeServer :: Env -> String -> String -> BS.ByteString -> IO FilePath
fakeServer env name version output = do
    let path = envDir env </> name
        outFile = path ++ ".out"
    BS.writeFile outFile output
    installFake (envExe env) path (FakeServer version outFile)

-- | Create the temporary directory and the fake commands, put the fake
-- commands first in @PATH@, make the temporary directory the working
-- directory and make the child processes of the test executable run as the
-- RPC server.
--
-- The tests run @sudo@, @su@, @ssh@ and @scp@ commands. Raise an exception
-- if @PATH@ does not find the fake command for any of them, so that a test
-- never runs the real one.
--
-- With the temporary directory as the working directory, a relative path or
-- a glob in a wrongly quoted command refers to the files of the temporary
-- directory only.
setupEnv :: IO Env
setupEnv = do
    origDir <- getCurrentDirectory
    tmp <- getTemporaryDirectory
    pid <- getCurrentPid
    exe <- getExecutablePath
    let dir = tmp </> ("simple-rpc-test-" ++ show pid)
        bin = dir </> "bin"
        home = dir </> "home"
        logFile = dir </> "fake.log"
    createDirectory dir
    createDirectory bin
    createDirectory home
    writeFile logFile ""
    fakes <-
        mapM
            (\(cmd, fake) -> (cmd,) <$> installFake exe (bin </> cmd) fake)
            [ ("ssh", FakeSsh), ("scp", FakeScp)
            , ("sudo", FakeSudo), ("su", FakeSu)
            ]
    path <- maybe "" id <$> lookupEnv "PATH"
    setEnv "PATH" (bin ++ searchPathSeparator : path)
    setEnv "SIMPLE_RPC_FAKE_HOME" home
    setEnv "SIMPLE_RPC_FAKE_LOG" logFile
    uncurry setEnv serverMode
    -- Used in a test path, must expand to an empty string
    unsetEnv "SIMPLE_RPC_UNSET"
    forM_ fakes $ \(cmd, fakeExe) -> do
        found <- findExecutable cmd
        when (found /= Just fakeExe)
            $ ioError $ userError
            $ "setupEnv: PATH finds " ++ show found ++ " for " ++ cmd
                ++ ", not the fake " ++ show fakeExe
    setCurrentDirectory dir
    pure $ Env dir origDir exe logFile

cleanupEnv :: Env -> IO ()
cleanupEnv env = do
    setCurrentDirectory (envOrigDir env)
    removeDirectoryRecursive (envDir env)

-- | The lines of the fake command log.
readFakeLog :: Env -> IO [String]
readFakeLog env = lines <$> readFileFs (envFakeLog env)

clearFakeLog :: Env -> IO ()
clearFakeLog env = writeFileFs (envFakeLog env) ""

-------------------------------------------------------------------------------
-- Server process
-------------------------------------------------------------------------------

-- | @runServerRaw env extraEnv args input@ runs the test executable as the
-- RPC server with @args@, the environment variables @extraEnv@ added, and
-- @input@ as stdin. Returns the exit code, stdout and stderr.
runServerRaw
    :: Env
    -> [(String, String)]
    -> [String]
    -> BS.ByteString
    -> IO (ExitCode, BS.ByteString, BS.ByteString)
runServerRaw env extraEnv args input = do
    env0 <- getEnvironment
    let vars = serverMode : extraEnv
        env1 = vars ++ [kv | kv@(k, _) <- env0, k `notElem` map fst vars]
        cp =
            (proc (envExe env) args)
                { std_in = CreatePipe
                , std_out = CreatePipe
                , std_err = CreatePipe
                , Process.env = Just env1
                }
    (mIn, mOut, mErr, ph) <- createProcess cp
    case (mIn, mOut, mErr) of
        (Just hin, Just hout, Just herr) -> do
            mapM_ (`hSetBinaryMode` True) [hin, hout, herr]
            outVar <- newEmptyMVar
            errVar <- newEmptyMVar
            _ <- forkIO $ BS.hGetContents hout >>= putMVar outVar
            _ <- forkIO $ BS.hGetContents herr >>= putMVar errVar
            BS.hPut hin input `catch` \(_ :: IOException) -> pure ()
            hClose hin `catch` \(_ :: IOException) -> pure ()
            out <- takeMVar outVar
            err <- takeMVar errVar
            code <- waitForProcess ph
            pure (code, out, err)
        _ -> error "runServerRaw: createProcess did not create pipes"

-------------------------------------------------------------------------------
-- Generators
-------------------------------------------------------------------------------

-- | Generate a JSON value with integral numbers, so that a value survives
-- an encode and decode round trip.
genValue :: Gen Value
genValue = QC.sized go

    where

    go :: Int -> Gen Value
    go n
        | n <= 1 = QC.oneof leaves
        | otherwise =
            let m = n `div` 4
                pair = (,) . Key.fromString <$> QC.arbitrary <*> go m
             in QC.oneof
                    $ leaves
                    ++ [ toJSON <$> QC.resize m (QC.listOf (go m))
                       , Json.object <$> QC.resize m (QC.listOf pair)
                       ]

    leaves =
        [ pure Json.Null
        , Json.Bool <$> QC.arbitrary
        , Json.Number . fromIntegral <$> (QC.arbitrary :: Gen Integer)
        , toJSON <$> (QC.arbitrary :: Gen String)
        ]

-- | A Unicode string without the surrogate code points, which 'toJSON'
-- replaces with U+FFFD.
genUnicodeString :: Gen String
genUnicodeString =
    QC.listOf
        $ QC.oneof
            [ QC.arbitrary `QC.suchThat` notSurrogate
            , QC.elements "\233\8594\128512\0\n\"\\"
            ]

    where

    notSurrogate c = c < '\xD800' || c > '\xDFFF'
