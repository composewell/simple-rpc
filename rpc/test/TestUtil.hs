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
    , writeScript
    , runServerRaw
    , genValue
    ) where

--------------------------------------------------------------------------------
-- Imports
--------------------------------------------------------------------------------

import Control.Concurrent (forkIO)
import Control.Concurrent.MVar (newEmptyMVar, putMVar, takeMVar)
import Control.Exception (IOException, catch, evaluate)
import Data.Aeson (Value, toJSON)
import System.Directory
    ( Permissions, createDirectory, getPermissions, getTemporaryDirectory
    , removeDirectoryRecursive, setOwnerExecutable, setPermissions )
import System.Environment
    (getEnvironment, getExecutablePath, lookupEnv, setEnv)
import System.Exit (ExitCode)
import System.FilePath ((</>))
import System.IO (hClose, hSetBinaryMode)
import System.Posix.Process (getProcessID)
import System.Process
    ( CreateProcess(..), StdStream(..), createProcess, proc
    , waitForProcess )
import Test.QuickCheck (Gen)

import qualified Data.Aeson as Json
import qualified Data.Aeson.Key as Key
import qualified Data.ByteString as BS
import qualified Test.QuickCheck as QC

--------------------------------------------------------------------------------
-- Environment
--------------------------------------------------------------------------------

-- | The name and value of the environment variable that runs the test
-- executable as the RPC server.
serverMode :: (String, String)
serverMode = ("SIMPLE_RPC_TEST_MODE", "server")

data Env =
    Env
        { envDir :: FilePath     -- ^ temporary directory for the tests
        , envExe :: FilePath     -- ^ path of the test executable
        , envFakeLog :: FilePath -- ^ invocations of the fake commands
        }

fakeSsh :: String
fakeSsh = unlines
    [ "#!/bin/sh"
    , "# Run the remote command with sh in $SIMPLE_RPC_FAKE_HOME."
    , logArgs "ssh"
    , "while [ $# -gt 0 ]; do"
    , "    case \"$1\" in"
    , "        -T) shift ;;"
    , "        -p|-o) shift 2 ;;"
    , "        -*) echo \"fake ssh: unsupported option $1\" >&2; exit 255 ;;"
    , "        *) break ;;"
    , "    esac"
    , "done"
    , "shift"
    , "cd \"$SIMPLE_RPC_FAKE_HOME\" || exit 255"
    , "exec sh -c \"$*\""
    ]

fakeScp :: String
fakeScp = unlines
    [ "#!/bin/sh"
    , "# scp -P PORT SRC HOST:DST, copy locally."
    , logArgs "scp"
    , "[ \"$1\" = \"-P\" ] && shift 2"
    , "src=$1"
    , "dst=${2#*:}"
    , "case \"$dst\" in"
    , "    /*) ;;"
    , "    *) dst=\"$SIMPLE_RPC_FAKE_HOME/$dst\" ;;"
    , "esac"
    , "exec cp \"$src\" \"$dst\""
    ]

fakeSudo :: String
fakeSudo = unlines
    [ "#!/bin/sh"
    , logArgs "sudo"
    , "[ \"$1\" = \"--\" ] && shift"
    , "exec \"$@\""
    ]

fakeSu :: String
fakeSu = unlines
    [ "#!/bin/sh"
    , "# su USER -c CMD"
    , logArgs "su"
    , "[ \"$2\" = \"-c\" ] || { echo \"fake su: usage\" >&2; exit 1; }"
    , "exec sh -c \"$3\""
    ]

-- | Shell code to append the command name and its arguments, each in
-- brackets, as a line to the fake log.
logArgs :: String -> String
logArgs name =
    "{ printf '" ++ name ++ "'; for a in \"$@\"; do printf ' [%s]' \"$a\"; \
    \done; echo; } >> \"$SIMPLE_RPC_FAKE_LOG\""

-- | Write an executable script.
writeScript :: FilePath -> String -> IO ()
writeScript path content = do
    writeFile path content
    p :: Permissions <- getPermissions path
    setPermissions path (setOwnerExecutable True p)

-- | Create the temporary directory and the fake commands, put the fake
-- commands first in @PATH@ and make the child processes of the test
-- executable run as the RPC server.
setupEnv :: IO Env
setupEnv = do
    tmp <- getTemporaryDirectory
    pid <- getProcessID
    exe <- getExecutablePath
    let dir = tmp </> ("simple-rpc-test-" ++ show pid)
        bin = dir </> "bin"
        home = dir </> "home"
        logFile = dir </> "fake.log"
    createDirectory dir
    createDirectory bin
    createDirectory home
    writeFile logFile ""
    writeScript (bin </> "ssh") fakeSsh
    writeScript (bin </> "scp") fakeScp
    writeScript (bin </> "sudo") fakeSudo
    writeScript (bin </> "su") fakeSu
    path <- maybe "" id <$> lookupEnv "PATH"
    setEnv "PATH" (bin ++ ":" ++ path)
    setEnv "SIMPLE_RPC_FAKE_HOME" home
    setEnv "SIMPLE_RPC_FAKE_LOG" logFile
    uncurry setEnv serverMode
    pure $ Env dir exe logFile

cleanupEnv :: Env -> IO ()
cleanupEnv env = removeDirectoryRecursive (envDir env)

-- | The lines of the fake command log.
readFakeLog :: Env -> IO [String]
readFakeLog env = do
    s <- readFile (envFakeLog env)
    _ <- evaluate (length s)
    pure (lines s)

clearFakeLog :: Env -> IO ()
clearFakeLog env = writeFile (envFakeLog env) ""

--------------------------------------------------------------------------------
-- Server process
--------------------------------------------------------------------------------

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
                , env = Just env1
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

--------------------------------------------------------------------------------
-- Generators
--------------------------------------------------------------------------------

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
        , (\x -> Json.Number (fromIntegral x)) <$> (QC.arbitrary :: Gen Integer)
        , toJSON <$> (QC.arbitrary :: Gen String)
        ]
