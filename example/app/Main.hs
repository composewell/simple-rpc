-- RPCIFY_MODULE
-- EXCLUDING: main

module Main
    ( main
    ) where

import Data.Function ((&))
import System.Environment (getExecutablePath)

import qualified Module1 as Module1
import qualified Module2 as Module2

import Simple.RPC.Server
import Simple.RPC.Client

exeVersion :: String
exeVersion = "0.1.1"

remoteExe :: Executable
remoteExe = Executable "/opt/ssh-rpc/test" exeVersion

localhost :: SSHConfig
localhost = ("user@localhost", "22")

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

top :: IO ()
top = do
    exePath <- getExecutablePath
    let exe = Executable exePath exeVersion
    run bottom
    call helloWorld (with (exec exe & asUser "user"))
    call Module2.printFromModule2 (with (exec exe & asUser "user")) "Hello"
    call Module2.printFromModule2
        (with (exec remoteExe & onSSH localhost & asUser "cw-portal"))
        "World"

main :: IO ()
main =
    mainWith
        exeVersion
        [ evaluator Module1.printFromModule1
        , evaluator Module2.printFromModule2
        , evaluator top
        , evaluator bottom
        , evaluator helloWorld
        ]
