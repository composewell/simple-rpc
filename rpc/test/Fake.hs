-- | Fake commands for the tests: @ssh@, @scp@, @sudo@, @su@ and RPC
-- servers with a given behaviour.
--
-- A fake command is a copy of the test executable with a config file next
-- to it. When the test executable starts, 'lookupFake' finds the config
-- file of the running copy, and 'runFake' runs the fake command instead of
-- the tests. A copy is used instead of a script because Windows runs only
-- executables, and instead of a link because a link reports the path of
-- the linked file as the executable path.
--
-- The fake @ssh@ runs the remote command with @sh -c@, the same as the
-- shell of the remote user, so that the tests check the shell quoting of
-- the commands. The fake @scp@ writes the remote file through @sh@ for the
-- same view of the remote file system.
--
module Fake
    ( Fake(..)
    , installFake
    , lookupFake
    , runFake
    , writeFileFs
    , appendFileFs
    , readFileFs
    ) where

-------------------------------------------------------------------------------
-- Imports
-------------------------------------------------------------------------------

import Control.Exception (evaluate)
import GHC.Environment (getFullArgs)
import System.Directory
    (copyFile, doesFileExist, exeExtension, getPermissions, setOwnerExecutable
    , setPermissions)
import System.Environment (getEnv, getExecutablePath)
import System.Exit (ExitCode(..), exitWith)
import System.FilePath (dropExtension, takeExtension, (<.>))
import System.IO
    ( Handle, IOMode(..), hGetContents, hPutStr, hSetBinaryMode, hSetEncoding
    , hSetNewlineMode, noNewlineTranslation, stderr, stdin, stdout
    , withFile )
import System.Process
    ( CreateProcess(cwd, std_in), StdStream(..), createProcess, proc
    , rawSystem, waitForProcess )

import qualified Data.ByteString as BS
import qualified GHC.IO.Encoding as Encoding

-------------------------------------------------------------------------------
-- Files in the file system encoding
-------------------------------------------------------------------------------

-- | Set the file system encoding and no newline translation, so that a
-- 'String' from 'getArgs' is written as the bytes of the argument, in any
-- locale and on Windows.
setFsMode :: Handle -> IO ()
setFsMode h = do
    enc <- Encoding.getFileSystemEncoding
    hSetEncoding h enc
    hSetNewlineMode h noNewlineTranslation

withFileFs :: FilePath -> IOMode -> (Handle -> IO a) -> IO a
withFileFs path mode f = withFile path mode (\h -> setFsMode h >> f h)

writeFileFs :: FilePath -> String -> IO ()
writeFileFs path s = withFileFs path WriteMode (`hPutStr` s)

appendFileFs :: FilePath -> String -> IO ()
appendFileFs path s = withFileFs path AppendMode (`hPutStr` s)

readFileFs :: FilePath -> IO String
readFileFs path =
    withFileFs path ReadMode $ \h -> do
        s <- hGetContents h
        _ <- evaluate (length s)
        pure s

-------------------------------------------------------------------------------
-- Fake config
-------------------------------------------------------------------------------

data Fake
    = FakeSsh
    | FakeScp
    | FakeSudo
    | FakeSu
      -- | @FakeServer version output@ prints @version@ when called without
      -- arguments. Otherwise it reads stdin and prints the contents of the
      -- file @output@.
    | FakeServer String FilePath
      -- | @ExitServer version code@ prints @version@ when called without
      -- arguments. Otherwise it exits with @code@ without reading stdin.
    | ExitServer String Int
      -- | @ArgsServer exe args@ runs @exe@ without arguments when called
      -- without arguments. Otherwise it runs @exe args@, ignoring its own
      -- arguments.
    | ArgsServer FilePath [String]
      -- | @RecordServer exe file@ appends a line @call@ and a line for each
      -- of its arguments to @file@, then runs @exe@ with the arguments.
    | RecordServer FilePath FilePath
    deriving (Show, Read)

-- | The config file of the fake command at the path.
configPath :: FilePath -> FilePath
configPath exe =
    base <.> "fake"

    where

    base =
        if not (null exeExtension) && takeExtension exe == '.' : exeExtension
        then dropExtension exe
        else exe

-- | @installFake testExe path fake@ copies the test executable @testExe@ to
-- @path@, with the executable extension of the OS added, and writes the
-- config file for @fake@. Returns the path of the fake command.
installFake :: FilePath -> FilePath -> Fake -> IO FilePath
installFake testExe path fake = do
    let exe = path <.> exeExtension
    copyFile testExe exe
    p <- getPermissions exe
    setPermissions exe (setOwnerExecutable True p)
    -- show escapes the non-ASCII characters, the file is ASCII
    writeFile (configPath exe) (show fake)
    pure exe

-- | The fake config of the running executable, if it is a fake command.
lookupFake :: IO (Maybe Fake)
lookupFake = do
    cfg <- configPath <$> getExecutablePath
    exists <- doesFileExist cfg
    if exists
    then Just . read <$> readFile cfg
    else pure Nothing

-------------------------------------------------------------------------------
-- Fake commands
-------------------------------------------------------------------------------

-- | Append the command name and its arguments, each in brackets, as a line
-- to the file in @SIMPLE_RPC_FAKE_LOG@.
logCall :: String -> [String] -> IO ()
logCall name args = do
    logFile <- getEnv "SIMPLE_RPC_FAKE_LOG"
    appendFileFs logFile
        $ unwords (name : map (\a -> "[" ++ a ++ "]") args) ++ "\n"

failWith :: Int -> String -> IO a
failWith code msg = do
    hPutStr stderr (msg ++ "\n")
    exitWith (ExitFailure code)

-- | Run the command with the standard streams of this process and exit with
-- its exit code.
runAndExit :: FilePath -> [String] -> IO a
runAndExit exe args = rawSystem exe args >>= exitWith

-- | Run @sh -c cmd shArgs@ in the fake home directory and exit with its exit
-- code.
runSh :: Maybe Handle -> String -> [String] -> IO a
runSh mIn cmd shArgs = do
    home <- getEnv "SIMPLE_RPC_FAKE_HOME"
    let cp =
            (proc "sh" ("-c" : cmd : shArgs))
                { cwd = Just home
                , std_in = maybe Inherit UseHandle mIn
                }
    (_, _, _, ph) <- createProcess cp
    waitForProcess ph >>= exitWith

-- | @ssh [-T] [-p PORT] [-o OPTION] HOST CMD...@ runs @CMD@ with @sh -c@ in
-- @SIMPLE_RPC_FAKE_HOME@. Like ssh, the words of @CMD@ are joined with
-- spaces.
fakeSsh :: [String] -> IO a
fakeSsh args0 =
    go args0

    where

    go ("-T" : rest) = go rest
    go ("-p" : _ : rest) = go rest
    go ("-o" : _ : rest) = go rest
    go (opt@('-' : _) : _) = failWith 255 ("fake ssh: unsupported " ++ opt)
    go (_host : cmd) = runSh Nothing (unwords cmd) []
    go [] = failWith 255 "fake ssh: no host"

-- | @scp [-P PORT] SRC HOST:DST@ writes the contents of @SRC@ to @DST@ with
-- @sh@ in @SIMPLE_RPC_FAKE_HOME@.
fakeScp :: [String] -> IO a
fakeScp args0 =
    case dropPort args0 of
        [src, dst] ->
            withFile src ReadMode $ \h ->
                runSh (Just h) "cat > \"$1\"" ["sh", remotePath dst]
        _ -> failWith 1 "fake scp: usage: scp [-P PORT] SRC HOST:DST"

    where

    dropPort ("-P" : _ : rest) = rest
    dropPort args = args

    remotePath = drop 1 . dropWhile (/= ':')

-- | @sudo [--] CMD ARGS...@ runs @CMD ARGS@ as the same user.
fakeSudo :: [String] -> IO a
fakeSudo args0 =
    case args0 of
        ("--" : cmd : args) -> runAndExit cmd args
        (cmd : args) -> runAndExit cmd args
        [] -> failWith 1 "fake sudo: no command"

-- | @su USER -c CMD@ runs @sh -c CMD@ as the same user.
fakeSu :: [String] -> IO a
fakeSu args =
    case args of
        [_user, "-c", cmd] -> runAndExit "sh" ["-c", cmd]
        _ -> failWith 1 "fake su: usage: su USER -c CMD"

printVersion :: String -> IO a
printVersion version = do
    hSetBinaryMode stdout True
    hPutStr stdout version
    exitWith ExitSuccess

-- | Run the fake command. The arguments include the RTS options such as
-- @+RTS -A2m -RTS@: the test executable accepts RTS options, and a fake
-- command passes them on as the real command would.
runFake :: Fake -> IO a
runFake fake = do
    -- The first one is the program name
    args <- drop 1 <$> getFullArgs
    case fake of
        FakeSsh -> logCall "ssh" args >> fakeSsh args
        FakeScp -> logCall "scp" args >> fakeScp args
        FakeSudo -> logCall "sudo" args >> fakeSudo args
        FakeSu -> logCall "su" args >> fakeSu args
        FakeServer version output
            | null args -> printVersion version
            | otherwise -> do
                hSetBinaryMode stdin True
                hSetBinaryMode stdout True
                _ <- BS.getContents
                BS.readFile output >>= BS.putStr
                exitWith ExitSuccess
        ExitServer version code
            | null args -> printVersion version
            | otherwise -> exitWith (ExitFailure code)
        ArgsServer exe serverArgs
            | null args -> runAndExit exe []
            | otherwise -> runAndExit exe serverArgs
        RecordServer exe file -> do
            appendFileFs file (unlines ("call" : args))
            runAndExit exe args
