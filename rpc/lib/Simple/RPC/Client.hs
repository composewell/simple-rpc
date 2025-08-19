{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE RecordWildCards #-}

module Simple.RPC.Client
    (
      Executable(..)
    , SSHConfig
    , Username
    , pipe
    , withH
    , with
    , asUser
    , onSSH
    , exec
    , sudo

    -- Backward compatibility
    , mkShCommand
    , runAt
    , runAs
    , runAtAs
    , installOnRemote
    ) where

--------------------------------------------------------------------------------
-- Imports
--------------------------------------------------------------------------------

import System.IO (stdout, Handle, hPutStrLn)
import System.IO.Unsafe (unsafePerformIO)
import Control.Exception (SomeException, throwIO, try, Exception)
import Control.Monad (when)
import Data.Function ((&))
import Data.Word (Word8)
import Streamly.Internal.Unicode.String (str)
import System.FilePath (takeDirectory)
import Streamly.Data.Array (Array)

import qualified Data.IORef as IORef
import qualified Streamly.Data.Stream as Stream
import qualified Streamly.Data.Fold as Fold
import qualified Streamly.FileSystem.Handle as FH hiding (read)
import qualified Streamly.Internal.Unicode.Stream as Unicode
import qualified Streamly.Internal.System.Command as Cmd
import qualified Streamly.Internal.System.Process as Proc

import Simple.RPC.Types
import Prelude hiding (putStrLn, print)

--------------------------------------------------------------------------------
-- Utils
--------------------------------------------------------------------------------

pipeChunksEither
    :: String
    -> Stream.Stream IO (Array Word8)
    -> Stream.Stream IO (Either (Array Word8) (Array Word8))
pipeChunksEither = Cmd.pipeWith Proc.pipeChunksEither

versionGuard :: Handle -> Maybe SSHConfig -> Executable -> IO ()
versionGuard outH mSshConf exe = do
    let exePath = executablePath exe
        exeVersionExpected = executableVersion exe
    let cmdVersionCheck =
            case mSshConf of
                Nothing -> [str|#{exePath}|]
                Just (addr, port) ->
                    [str|ssh -T -p #{port} -o "StrictHostKeyChecking off" #{addr} #{exePath}|]
    hPutStrLn outH $ showPair "CMD" cmdVersionCheck
    exeVersion <- Cmd.toString cmdVersionCheck
    let prefix =
            case mSshConf of
                Nothing -> ""
                Just (addr, port) -> [str|#{addr}:#{port}:|]
    when (not (exeVersion == exeVersionExpected)) $ do
        hPutStrLn outH $ showPair "ERR" [str|Version mismatch for #{prefix}#{exePath}|]
        hPutStrLn outH $ showPair "ERR" [str|Expected: #{exeVersionExpected}|]
        hPutStrLn outH $ showPair "ERR" [str|Got: #{exeVersion}|]
        error "VersionMismatch"

tracing :: Handle -> String -> String -> IO String
tracing outH tag val = hPutStrLn outH (showPair tag val) >> pure val

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

pipe :: Handle -> Pipe -> Runner IntermediateRep
pipe outH using cmd input = do
    hPutStrLn outH $ showPair "CMD" cmd
    hPutStrLn outH $ showTag "INP"
    toBinStream input & Stream.fold (FH.write outH)
    hPutStrLn outH ""

    errRef <- IORef.newIORef []

    actionRes <- try $
        toChunkStream input
            & using cmd
            & bufferLefts errRef
            & Unicode.decodeUtf8Chunks
            & Stream.foldMany (Fold.takeEndBy_ (== '\n') Fold.toList)
            & Stream.fold (Fold.lmapM (tracing outH "STDOUT") Fold.latest)

    chunkBufferReversed <- IORef.readIORef errRef
    let errBuffer = reverse chunkBufferReversed
    let errStream = errBuffer & Stream.fromList

    errStream & Unicode.decodeUtf8Chunks
       & Stream.foldMany (Fold.takeEndBy_ (== '\n') Fold.toList)
       & Stream.fold (Fold.drainMapM (tracing outH "STDERR"))
    case actionRes of
        Left (err :: SomeException) -> throwIO err
        Right Nothing -> throwIO EmptyOutput
        Right (Just val) -> pure $ irFromString val

withH :: Handle -> RunningConfig -> Runner IntermediateRep
withH outH (RunningConfig{..}) actionName input = do
    versionGuard outH rcSSH rcExe
    let cmd =
              sshWrapper rcSSH
            $ userWrapper rcUser
            $ sudoWrapper rcSudo
            $ exeAction rcExe
    hPutStrLn outH $ showPair "ACT" actionName
    pipe outH pipeChunksEither cmd input

    where
    sshWrapper Nothing val = val
    sshWrapper (Just (addr, port)) val =
        let quotedVal = show val
         in [str|ssh -T -p #{port} -o "StrictHostKeyChecking off" #{addr} #{quotedVal}|]

    userWrapper Nothing val = val
    userWrapper (Just username) val =
        let quotedVal = show val
         in [str|sudo su #{username} -c #{quotedVal}|]

    sudoWrapper False val = val
    sudoWrapper True val = [str|sudo #{val}|]

    exeAction exeObj =
        let ePath = executablePath exeObj
         in [str|#{ePath} #{actionName}|]

with :: RunningConfig -> Runner IntermediateRep
with = withH stdout

--------------------------------------------------------------------------------
-- Backward compatibility
--------------------------------------------------------------------------------

mkShCommand
    :: RpcSymbol IntermediateRep b -> Executable -> [IntermediateRep] -> String
mkShCommand sym exe inpList =
    let cmdName = symbol (evaluator sym)
        epath = executablePath exe
        inpStr =
            toIntermediateRep inpList
                & toBinStream
                & Unicode.decodeUtf8'
                & Stream.toList
                & unsafePerformIO
        inpStrQuoted = show inpStr
        cmd = [str|echo #{inpStrQuoted} | #{epath} #{cmdName}|]
        cmdQuoted = show cmd
     in [str|sh -c #{cmdQuoted}|]

runAt :: Executable -> SSHConfig -> Runner IntermediateRep
runAt exe sshConf = with (exec exe & onSSH sshConf)

runAs :: Executable -> Username -> Runner IntermediateRep
runAs exe uname = with (exec exe & asUser uname)

runAtAs :: Executable -> SSHConfig -> Username -> Runner IntermediateRep
runAtAs exe sshConf uname = with (exec exe & onSSH sshConf & asUser uname)

-- NOTE: This should be removed from here. Leave the installation to the user.
installOnRemote :: Executable -> SSHConfig -> FilePath -> IO ()
installOnRemote localExe sshConf@(addr, port) remoteExePath = do
    let exePath = executablePath localExe
        remoteExeDir = takeDirectory remoteExePath
    versionGuard stdout Nothing localExe
    Cmd.toStdout $ mkSSHCmd sshConf [str|sudo mkdir -p #{remoteExeDir}|]
    Cmd.toStdout $ mkSSHCmd sshConf [str|sudo chmod a+rw #{remoteExeDir}|]
    Cmd.toStdout [str|scp -P #{port} #{exePath} #{addr}:#{remoteExePath}|]
    Cmd.toStdout $ mkSSHCmd sshConf [str|sudo chmod a+r #{remoteExeDir}|]
    Cmd.toStdout $ mkSSHCmd sshConf [str|sudo chmod a+x #{remoteExePath}|]

    where

    mkSSHCmd (a, p) cmd =
        let qCmd = show cmd
         in [str|ssh -T -p #{p} -o "StrictHostKeyChecking off" #{a} #{qCmd}|]
