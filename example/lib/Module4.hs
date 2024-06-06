module Module4
    ( top
    ) where

import Data.Function ((&))
import System.Environment (getExecutablePath)

import qualified RPC.Module2 as Module2
import qualified RPC.Module3 as Module3

import Module0
import Simple.RPC.Client
import Simple.RPC.Server

top :: IO ()
top = do
    exePath <- getExecutablePath
    let exe = Executable exePath exeVersion
    run Module3.bottom
    call Module3.helloWorld (with (exec exe & asUser "user"))
    call Module2.printFromModule2 (with (exec exe & asUser "user")) "Hello"
    call Module2.printFromModule2
        (with (exec remoteExe & onSSH localhost & asUser "cw-portal"))
        "World"
