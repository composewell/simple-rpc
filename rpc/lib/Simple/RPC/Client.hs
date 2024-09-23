{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE RecordWildCards #-}

module Simple.RPC.Client
    (
      Executable(..)
    , SSHConfig
    , Username
    , with
    , asUser
    , onSSH
    , exec

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

import System.IO (stdout)
import System.IO.Unsafe (unsafePerformIO)
import Control.Exception (SomeException, throwIO, try)
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

--------------------------------------------------------------------------------
-- Utils
--------------------------------------------------------------------------------

pipeChunksEither
    :: String
    -> Stream.Stream IO (Array Word8)
    -> Stream.Stream IO (Either (Array Word8) (Array Word8))
pipeChunksEither = Cmd.pipeWith Proc.pipeChunksEither

withLog :: (String -> IO a) -> String -> IO a
withLog f cmd = do
    printWith "CMD" cmd
    f cmd

versionGuard :: Maybe SSHConfig -> Executable -> IO ()
versionGuard mSshConf exe = do
    let exePath = executablePath exe
        exeVersionExpected = executableVersion exe
    let cmdVersionCheck =
            case mSshConf of
                Nothing -> [str|#{exePath}|]
                Just (addr, port) ->
                    [str|ssh -T -p #{port} -o "StrictHostKeyChecking off" #{addr} #{exePath}|]
    exeVersion <- withLog Cmd.toString cmdVersionCheck
    let prefix =
            case mSshConf of
                Nothing -> ""
                Just (addr, port) -> [str|#{addr}:#{port}:|]
    when (not (exeVersion == exeVersionExpected)) $ do
        printWith "ERR" [str|Version mismatch for #{prefix}#{exePath}|]
        printWith "ERR" [str|Expected: #{exeVersionExpected}|]
        printWith "ERR" [str|Got: #{exeVersion}|]
        error "VersionMismatch"

tracing :: String -> String -> IO String
tracing tag val = printWith tag val >> pure val

with :: RunningConfig -> Runner IntermediateRep
with (RunningConfig{..}) actionName input = do
    versionGuard rcSSH rcExe
    let cmd = sshWrapper rcSSH (userWrapper rcUser (exeAction rcExe))

    printWith "ACTION" actionName

    printTag "INPUT"
    toBinStream input & Stream.fold (FH.write stdout)
    putStrLn ""

    printWith "RUNNING" cmd

    ref <- IORef.newIORef []
    actionRes <- try $
        toChunkStream input
            & pipeChunksEither cmd
            & Stream.fold (Fold.drainMapM (\c -> IORef.modifyIORef ref (c:)))

    chunkBufferReversed <- IORef.readIORef ref
    let buffer = reverse chunkBufferReversed

    let outStream = buffer & Stream.fromList & Stream.catRights
        errStream = buffer & Stream.fromList & Stream.catLefts

    res <-
        outStream & Unicode.decodeUtf8Chunks
           & Stream.foldMany (Fold.takeEndBy_ (== '\n') Fold.toList)
           & Stream.fold (Fold.lmapM (tracing "STDOUT") Fold.latest)

    errStream & Unicode.decodeUtf8Chunks
       & Stream.foldMany (Fold.takeEndBy_ (== '\n') Fold.toList)
       & Stream.fold (Fold.drainMapM (tracing "STDERR"))

    case actionRes of
        Left (err :: SomeException) -> throwIO err
        Right _ -> pure ()

    case res of
        Nothing -> error "No output recieved"
        Just val -> pure $ irFromString val

    where

    sshWrapper Nothing val = val
    sshWrapper (Just (addr, port)) val =
        let quotedVal = show val
         in [str|ssh -T -p #{port} -o "StrictHostKeyChecking off" #{addr} #{quotedVal}|]

    userWrapper Nothing val = val
    userWrapper (Just username) val =
        let quotedVal = show val
         in [str|sudo su #{username} -c #{quotedVal}|]

    exeAction exeObj =
        let ePath = executablePath exeObj
         in [str|#{ePath} #{actionName}|]

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

installOnRemote :: Executable -> SSHConfig -> FilePath -> IO ()
installOnRemote localExe sshConf@(addr, port) remoteExePath = do
    let exePath = executablePath localExe
        remoteExeDir = takeDirectory remoteExePath
    versionGuard Nothing localExe
    withLog Cmd.toStdout $ mkSSHCmd sshConf [str|sudo mkdir -p #{remoteExeDir}|]
    withLog Cmd.toStdout $ mkSSHCmd sshConf [str|sudo chmod a+rw #{remoteExeDir}|]
    withLog Cmd.toStdout [str|scp -P #{port} #{exePath} #{addr}:#{remoteExePath}|]
    withLog Cmd.toStdout $ mkSSHCmd sshConf [str|sudo chmod a+r #{remoteExeDir}|]
    withLog Cmd.toStdout $ mkSSHCmd sshConf [str|sudo chmod a+x #{remoteExePath}|]

    where

    mkSSHCmd (a, p) cmd =
        let qCmd = show cmd
         in [str|ssh -T -p #{p} -o "StrictHostKeyChecking off" #{a} #{qCmd}|]
