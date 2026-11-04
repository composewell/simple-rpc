{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE RecordWildCards #-}

-- | Call the RPC endpoints of a server executable on the local host or, using
-- ssh, on a remote host.
--
module Simple.RPC.Client
    (
    -- * RPC call
      runAt
    , run

    -- * RPC call config
    , Executable(..) -- XXX RpcImage, use sha256 instead of version
    , RunningConfig(..) -- XXX RpcConfig or just Config
    , defaultConfig
    , setImage
    , setImageFlags
    , setRemote
    , setUser
    , useSudo
    , setLogger

    -- * Pretty Printing
    , showPair -- XXX this does not belong in simple-rpc

    -- * Copy RPC Executable
    , installOnRemote
    ) where

-------------------------------------------------------------------------------
-- Imports
-------------------------------------------------------------------------------

import Control.Exception (SomeException, onException, throwIO, try)
import Control.Monad (when)
import Data.Char (isAsciiLower, isAsciiUpper, isDigit)
import Data.Function ((&))
import Data.Word (Word8)
import Streamly.Data.Array (Array)
import Streamly.Internal.Unicode.String (str)
import System.FilePath (takeDirectory)

import qualified Data.IORef as IORef
import qualified Streamly.Internal.Data.Array as Array (compactEndByLn_)
import qualified Streamly.Data.Fold as Fold
import qualified Streamly.Data.Stream as Stream
import qualified Streamly.Internal.System.Process as Proc
import qualified Streamly.Internal.Unicode.Stream as Unicode

import Simple.RPC.Internal.Types

-------------------------------------------------------------------------------
-- Utils
-------------------------------------------------------------------------------

-- streamly-coreutils (Streamly.Coreutils.Sh) has the maintained definitions
-- of shellQuote and shellCommand; these are copies. Make changes there and
-- copy them here.

-- | Quote a string for a POSIX shell. A non-empty string of characters that
-- the shell does not interpret is returned unchanged.
shellQuote :: String -> String
shellQuote x
    | not (null x) && all safe x = x
    | otherwise = '\'' : concatMap escape x ++ "'"

    where

    escape '\'' = "'\\''"
    escape c = [c]

    safe c =
        isAsciiUpper c || isAsciiLower c || isDigit c || c `elem` "-_./=:,+@%"

-- | A command line for a POSIX shell, running the executable with the
-- arguments.
shellCommand :: FilePath -> [String] -> String
shellCommand exe args = unwords (map shellQuote (exe : args))

-- | @sshShell remote cmd@ runs the shell command line @cmd@ on the @remote@
-- (address, port) using ssh. ssh passes the command to the shell of the
-- remote user.
sshShell :: (String, String) -> String -> (FilePath, [String])
sshShell (addr, port) cmd =
    ("ssh", ["-T", "-p", port, "-o", "StrictHostKeyChecking off", addr, cmd])

-- | @sshCommand remote (exe, args)@ runs @exe args@ on the @remote@ (address,
-- port) using ssh.
sshCommand :: (String, String) -> (FilePath, [String]) -> (FilePath, [String])
sshCommand remote (exe, args) = sshShell remote (shellCommand exe args)

versionGuard
    :: (String -> IO ()) -> Maybe (String, String) -> Executable -> IO ()
versionGuard send mSshConf exe = do
    let exePath = executablePath exe
        exeVersionExpected = executableVersion exe
    let (cmd, args) = maybe id sshCommand mSshConf (exePath, [])
    send $ showPair "CMD" (shellCommand cmd args)
    exeVersion <- Proc.toString cmd args
    let prefix =
            case mSshConf of
                Nothing -> ""
                Just (addr, port) -> [str|#{addr}:#{port}:|]
    when (not (exeVersion == exeVersionExpected)) $ do
        send $ showPair "ERR" [str|Version mismatch for #{prefix}#{exePath}|]
        send $ showPair "ERR" [str|Expected: #{exeVersionExpected}|]
        send $ showPair "ERR" [str|Got: #{exeVersion}|]
        throwIO
            $ VersionMismatch
                [str|#{prefix}#{exePath}|] exeVersionExpected exeVersion

tracing :: (String -> IO ()) -> String -> String -> IO String
tracing send tag val = send (showPair tag val) >> pure val

traceLine :: (String -> IO ()) -> String -> Array Word8 -> IO (Array Word8)
traceLine send tag val = send (showPair tag (decodeUtf8Array val)) >> pure val

bufferLefts
    :: IORef.IORef [a] -> Stream.Stream IO (Either a b) -> Stream.Stream IO b
bufferLefts ref inp =
    Stream.mapM f inp & Stream.catMaybes

    where

    f (Left a) = IORef.modifyIORef ref (a:) >> pure Nothing
    f (Right b) = pure $ Just b

-- XXX We should allow the input to be a stream instead of being finite.
-- XXX We should not buffer the output, it should be returned as a stream.
-- XXX We should be able to use websockets instead of ssh.

-- | @pipe send exe args input@ - run the executable @exe@ with arguments
-- @args@ as a process providing @input@ on its stdin and returning the last
-- line of its output as result. i.e. it takes input from stdin and provides
-- output on stdout.
--
-- BUGs: the output JSON from the remote call must not have a newline '\n'
-- character. It will cause unexpected behavior. Also, the output must be
-- finite as it is collected in a buffer.
--
-- @send@ is a function for logging a String for debug and tracing.
pipe
    :: (String -> IO ())
    -> FilePath
    -> [String]
    -> IntermediateRep
    -> IO IntermediateRep
pipe send exe args input = do
    send $ showPair "CMD" (shellCommand exe args)
    send $ showPair "INP" ""
    send $ irToString input
    send ""

    errRef <- IORef.newIORef []

    actionRes <- try $
        toChunkStream input
            & Proc.pipeChunksEither exe args
            & bufferLefts errRef
            & Array.compactEndByLn_
            & Stream.fold (Fold.lmapM (traceLine send "STDOUT") Fold.latest)

    chunkBufferReversed <- IORef.readIORef errRef
    let errBuffer = reverse chunkBufferReversed
    let errStream = errBuffer & Stream.fromList

    errStream & Unicode.decodeUtf8Chunks
       & Stream.foldMany (Fold.takeEndBy_ (== '\n') Fold.toList)
       & Stream.fold (Fold.drainMapM (tracing send "STDERR"))
    case actionRes of
        Left (err :: SomeException) -> throwIO err
        Right Nothing -> throwIO EmptyOutput
        Right (Just val) -> do
            r <- irFromArray val
            case r of
                Left err -> throwIO (InvalidOutput err)
                Right x -> pure x

-- XXX the remote call should be more seamless. We should be able to wrap any
-- arbitrary code in runAt as long as its input and output are serializable.
-- Maybe we can design a Monad to implement the runAt call under the hood.

-- | Given an RPC server access specification create a Runner which takes an
-- endpoint call specification, to run the endpoint on the server and return
-- the result.
with :: RunningConfig -> Runner IntermediateRep
with (RunningConfig{..}) actionName input = do
    let send = rcLogger
    versionGuard send rcRemote rcImage
    let (exe, args) =
              maybe id sshCommand rcRemote
            $ userWrapper rcUser
            $ sudoWrapper rcSudo
            (executablePath rcImage, words rcImageFlags ++ [actionName])
    send $ showPair "ACT" actionName
    pipe send exe args input

    where

    -- XXX we should use either user wrapper or sudo wrapper, not both. If we
    -- are using userWrapper then we know we are sudo capable and can use sudo
    -- directly.

    -- Does not run the shell profile but runs the rc file. su passes the
    -- command to the shell of the user.
    userWrapper Nothing cmd = cmd
    userWrapper (Just username) (cmd, args) =
        ("sudo", ["su", username, "-c", shellCommand cmd args])

    sudoWrapper False cmd = cmd
    sudoWrapper True (cmd, args) = ("sudo", cmd : args)

-- | Make a RPC call at the remote host specified by the first argument.
--
-- The call runs the executable twice, without arguments to check its version,
-- and with the endpoint name to call the endpoint. The arguments are written
-- to the stdin of the server as a JSON array, the last line of the stdout of
-- the server is decoded as the result. The stderr of the server is buffered
-- in memory and logged after the server exits. The result is decoded before
-- the call returns.
--
-- On a remote host, the output of the shell startup files of the ssh user,
-- if any, is part of the version check output and causes a version mismatch.
--
-- The call raises:
--
-- * a 'VersionMismatch' exception if the server executable prints a version
-- other than 'executableVersion',
-- * a 'Streamly.Internal.System.Process.ProcessFailure' exception if the
-- server process exits with a non-zero exit code,
-- * an 'EmptyOutput' exception if the server does not print an output line,
-- * an 'InvalidOutput' exception if the last output line of the server is not
-- the JSON encoding of the result type.
--
-- With streamly-process 0.4.1, if the server process exits before it reads
-- all of the input and the input is larger than the pipe buffer, the call
-- raises an 'IOError' for the broken pipe instead of the
-- 'Streamly.Internal.System.Process.ProcessFailure' exception. The server
-- reads the whole input before it raises an exception for an unknown
-- endpoint.
--
-- /Dependencies/: none for a local call as the current user. 'setRemote'
-- needs @ssh@ on the local host and a POSIX shell for the ssh user on the
-- remote host. 'setUser' needs @sudo@, @su@ and a POSIX shell for the user.
-- 'useSudo' needs @sudo@. With 'setRemote', @sudo@ and @su@ run on the
-- remote host, otherwise on the local host.
runAt :: RunningConfig -> RpcSymbol IntermediateRep typ -> typ
runAt cfg sym = call sym (with cfg)

-- | Make a direct local call to the function. This is equivalent to calling
-- the function directly without going through any wrappers, serialization,
-- deserialization.
run :: RpcSymbol IntermediateRep typ -> typ
run = direct

-------------------------------------------------------------------------------
-- Backward compatibility
-------------------------------------------------------------------------------

-- NOTE: This should be removed from here. Leave the installation to the user.
-- XXX We can avoid sudo access or have a separate API which does not require
-- sudo access.

-- XXX scpRpcImage

-- | @installOnRemote localExe sshEndpoint remotePath@. Install the RPC
-- endpoint server executable on a remote machine:
--
-- * check the version of the local executable,
-- * copy it with @scp@ to a temporary file created by @mktemp@ on the remote
--   machine,
-- * create the directory of @remotePath@ with @sudo mkdir -p@, if it does not
--   exist, and copy the temporary file to @remotePath@ with
--   @sudo install -m 755@, the file is owned by root and is readable and
--   executable by all users,
-- * remove the temporary file.
--
-- The permissions of an existing directory are not changed. @install@
-- creates a new file, therefore an instance of the executable that is
-- running is not affected.
--
-- The local executable is copied as it is, the remote host must be able to
-- run the same file: the same OS and CPU architecture, and, unless the
-- executable is statically linked, compatible versions of the shared
-- libraries that it is linked with, e.g. glibc, libgmp and libffi. For
-- example, an executable built on macOS does not run on Linux, and one
-- built on a Linux distribution with a newer glibc may not run on another.
-- The version check runs the local executable, therefore it also has to
-- run on the local host. Not supported on a Windows client or a Windows
-- remote host.
--
-- Raise a 'VersionMismatch' exception if the local executable prints a
-- version other than 'executableVersion', and a
-- 'Streamly.Internal.System.Process.ProcessFailure' exception if a command
-- fails.
--
-- /Dependencies/: @ssh@ and @scp@ on the local host. A POSIX shell for the
-- ssh user, @mktemp@, @rm@, @mkdir@, @install@ and @sudo@ access on the
-- remote host.
installOnRemote :: Executable -> (String, String) -> FilePath -> IO ()
installOnRemote localExe sshConf@(addr, port) remoteExePath = do
    let exePath = executablePath localExe
        remoteExeDir = takeDirectory remoteExePath
    versionGuard putStrLn Nothing localExe
    tmpOut <- uncurry Proc.toString $ sshCommand sshConf ("mktemp", [])
    let tmp = takeWhile (/= '\n') tmpOut
        removeTmp =
            uncurry Proc.toStdout $ sshCommand sshConf ("rm", ["-f", tmp])
    flip onException removeTmp
        $ Proc.toStdout "scp"
            ["-P", port, scpLocalPath exePath, addr ++ ":" ++ tmp]
    let qTmp = shellQuote tmp
        install =
            shellCommand "sudo" ["mkdir", "-p", remoteExeDir]
                ++ " && "
                ++ shellCommand "sudo"
                    ["install", "-m", "755", tmp, remoteExePath]
                ++ "; r=$?; rm -f " ++ qTmp ++ "; exit $r"
    uncurry Proc.toStdout $ sshShell sshConf install

    where

    -- scp takes a path with a colon before any slash as a remote path, and a
    -- path that starts with a dash as an option.
    scpLocalPath p =
        case break (\c -> c == ':' || c == '/') p of
            (_, ':' : _) -> "./" ++ p
            ('-' : _, _) -> "./" ++ p
            _ -> p
