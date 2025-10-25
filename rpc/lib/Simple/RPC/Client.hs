{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE RecordWildCards #-}

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

--------------------------------------------------------------------------------
-- Imports
--------------------------------------------------------------------------------

import Control.Exception (SomeException, throwIO, try, Exception)
import Control.Monad (when)
import Data.Function ((&))
import Data.Word (Word8)
import Streamly.Data.Array (Array)
import Streamly.Data.Stream (Stream)
import Streamly.Internal.Unicode.String (str)
import System.FilePath (takeDirectory)

import qualified Data.IORef as IORef
import qualified Streamly.Data.Array as Array
import qualified Streamly.Data.Fold as Fold
import qualified Streamly.Data.Stream as Stream
import qualified Streamly.Internal.System.Command as Cmd
import qualified Streamly.Internal.System.Process as Proc
import qualified Streamly.Internal.Unicode.Stream as Unicode

import Simple.RPC.Internal.Types

--------------------------------------------------------------------------------
-- Utils
--------------------------------------------------------------------------------

type Pipe =
    String ->
    Stream IO (Array.Array Word8) ->
    Stream IO (Either (Array.Array Word8) (Array.Array Word8))

pipeChunksEither
    :: String
    -> Stream.Stream IO (Array Word8)
    -> Stream.Stream IO (Either (Array Word8) (Array Word8))
pipeChunksEither = Cmd.pipeWith Proc.pipeChunksEither

versionGuard :: (String -> IO ()) -> Maybe (String, String) -> Executable -> IO ()
versionGuard send mSshConf exe = do
    let exePath = executablePath exe
        exeVersionExpected = executableVersion exe
    let cmdVersionCheck =
            case mSshConf of
                Nothing -> [str|#{exePath}|]
                Just (addr, port) ->
                    [str|ssh -T -p #{port} -o "StrictHostKeyChecking off" #{addr} #{exePath}|]
    send $ showPair "CMD" cmdVersionCheck
    exeVersion <- Cmd.toString cmdVersionCheck
    let prefix =
            case mSshConf of
                Nothing -> ""
                Just (addr, port) -> [str|#{addr}:#{port}:|]
    when (not (exeVersion == exeVersionExpected)) $ do
        send $ showPair "ERR" [str|Version mismatch for #{prefix}#{exePath}|]
        send $ showPair "ERR" [str|Expected: #{exeVersionExpected}|]
        send $ showPair "ERR" [str|Got: #{exeVersion}|]
        error "VersionMismatch"

tracing :: (String -> IO ()) -> String -> String -> IO String
tracing send tag val = send (showPair tag val) >> pure val

bufferLefts
    :: IORef.IORef [a] -> Stream.Stream IO (Either a b) -> Stream.Stream IO b
bufferLefts ref inp =
    Stream.mapM f inp & Stream.catMaybes
    where
    f (Left a) = IORef.modifyIORef ref (a:) >> pure Nothing
    f (Right b) = pure $ Just b

data RpcException = EmptyOutput
    deriving (Show)

instance Exception RpcException

-- XXX We should allow the input to be a stream instead of being finite.
-- XXX We should not buffer the output, it should be returned as a stream.
-- XXX We should be able to use websockets instead of ssh.

-- | @pipe send using cmd input@ - execute @cmd@ (the @ssh@ command) as a
-- process providing @input@ on its stdin and returning the last line of its
-- output as result. i.e. it takes input from stdin and provides output on
-- stdout.
--
-- BUGs: the output JSON from the remote call must not have a newline '\n'
-- character. It will cause unexpected behavior. Also, the output must be
-- finite as it is collected in a buffer.
--
-- @send@ is a function for logging a String for debug and tracing.
pipe :: (String -> IO ()) -> Pipe -> Runner IntermediateRep
pipe send using cmd input = do
    send $ showPair "CMD" cmd
    send $ showPair "INP" ""
    send $ irToString input
    send ""

    errRef <- IORef.newIORef []

    actionRes <- try $
        toChunkStream input
            & using cmd
            & bufferLefts errRef
            & Unicode.decodeUtf8Chunks
            & Stream.foldMany (Fold.takeEndBy_ (== '\n') Fold.toList)
            & Stream.fold (Fold.lmapM (tracing send "STDOUT") Fold.latest)

    chunkBufferReversed <- IORef.readIORef errRef
    let errBuffer = reverse chunkBufferReversed
    let errStream = errBuffer & Stream.fromList

    errStream & Unicode.decodeUtf8Chunks
       & Stream.foldMany (Fold.takeEndBy_ (== '\n') Fold.toList)
       & Stream.fold (Fold.drainMapM (tracing send "STDERR"))
    case actionRes of
        Left (err :: SomeException) -> throwIO err
        Right Nothing -> throwIO EmptyOutput
        Right (Just val) -> pure $ irFromString val

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
    let cmd =
              sshWrapper rcRemote
            $ userWrapper rcUser
            $ sudoWrapper rcSudo
            $ exeAction rcImage
    send $ showPair "ACT" actionName
    pipe send pipeChunksEither cmd input

    where

    sshWrapper Nothing val = val
    sshWrapper (Just (addr, port)) val =
        let quotedVal = show val
         in [str|ssh -T -p #{port} -o "StrictHostKeyChecking off" #{addr} #{quotedVal}|]

    -- XXX we should use either user wrapper or sudo wrapper, not both. If we
    -- are using userWrapper then we know we are sudo capable and can use sudo
    -- directly.

    -- Does not run the shell profile but runs the rc file
    userWrapper Nothing val = val
    userWrapper (Just username) val =
        let quotedVal = show val
         in [str|sudo su #{username} -c #{quotedVal}|]

    sudoWrapper False val = val
    sudoWrapper True val = [str|sudo #{val}|]

    exeAction exeObj =
        let ePath = executablePath exeObj
         in [str|#{ePath} #{actionName}|]

-- | Make a RPC call at the remote host specified by the first argument.
runAt :: RunningConfig -> RpcSymbol IntermediateRep typ -> typ
runAt cfg sym = call sym (with cfg)

-- | Make a direct local call to the function. This is equivalent to calling
-- the function directly without going through any wrappers, serialization,
-- deserialization.
run :: RpcSymbol IntermediateRep typ -> typ
run = direct

--------------------------------------------------------------------------------
-- Backward compatibility
--------------------------------------------------------------------------------

-- NOTE: This should be removed from here. Leave the installation to the user.
-- XXX We can avoid sudo access or have a separate API which does not require
-- sudo access.

-- XXX scpRpcImage

-- | @installOnRemote localPath sshEndpoint remotePath@. Install the RPC
-- endpoint server executable on a remote machine using @scp@. Requires @sudo@
-- access to create the directory if it does not exist.
installOnRemote :: Executable -> (String, String) -> FilePath -> IO ()
installOnRemote localExe sshConf@(addr, port) remoteExePath = do
    let exePath = executablePath localExe
        remoteExeDir = takeDirectory remoteExePath
    versionGuard putStrLn Nothing localExe
    Cmd.toStdout $ mkSSHCmd sshConf [str|sudo mkdir -p #{remoteExeDir}|]

    -- XXX this is unsafe, gives write access to anyone.
    Cmd.toStdout $ mkSSHCmd sshConf [str|sudo chmod a+rw #{remoteExeDir}|]
    Cmd.toStdout [str|scp -P #{port} #{exePath} #{addr}:#{remoteExePath}|]
    -- XXX nned chmod o-w
    Cmd.toStdout $ mkSSHCmd sshConf [str|sudo chmod a+r #{remoteExeDir}|]
    -- XXX we do not need sudo for this.
    Cmd.toStdout $ mkSSHCmd sshConf [str|sudo chmod a+x #{remoteExePath}|]

    where

    mkSSHCmd (a, p) cmd =
        let qCmd = show cmd
         in [str|ssh -T -p #{p} -o "StrictHostKeyChecking off" #{a} #{qCmd}|]
