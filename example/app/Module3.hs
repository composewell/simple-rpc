module Module3
    ( helloWorld
    , bottom
    ) where

import Data.Function ((&))
import System.Environment (getExecutablePath)

import qualified RPC.Module1 as Module1
import qualified RPC.Module2 as Module2

import Module0
import Simple.RPC.Client
import Simple.RPC.Server

helloWorld :: IO ()
helloWorld = do
    putStr "Hello, World"

bottom :: IO ()
bottom = do
    exePath <- getExecutablePath
    let exe = Executable exePath exeVersion
    val <-
        call
            Module1.printFromModule1
            (with (exec exe & asUser "user")) 5 "Hello" False
    print val
    call
        Module2.printFromModule2
        (with (exec remoteExe & onSSH localhost))  "World"
