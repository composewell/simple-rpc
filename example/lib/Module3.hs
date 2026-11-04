module Module3
    ( helloWorld
    , bottom
    ) where

import Data.Function ((&))
import System.Environment (getExecutablePath)

import qualified Module1 as Module1
import qualified Module2 as Module2

import Module0
import Simple.RPC

helloWorld' :: IO ()
helloWorld' = do
    putStr "Hello, World"
$(rpcify 'helloWorld')

raw_bottom :: IO ()
raw_bottom = do
    exePath <- getExecutablePath
    let exe = Executable exePath exeVersion
    val <-
        runAt
            (defaultConfig exe & setUser "user")
            Module1.printFromModule1 5 "Hello" False
    print val
    runAt
        (defaultConfig remoteExe & setRemote localhost)
        Module2.printFromModule2 "World"
$(rpcExport "raw_bottom" "bottom")
