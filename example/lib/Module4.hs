module Module4
    ( top
    ) where

import Data.Function ((&))
import System.Environment (getExecutablePath)

import qualified Module2 as Module2
import qualified Module3 as Module3

import Module0
import Simple.RPC

top' :: IO ()
top' = do
    exePath <- getExecutablePath
    let exe = Executable exePath exeVersion
    run Module3.bottom
    runAt (defaultConfig exe & setUser "user") Module3.helloWorld
    runAt
        (defaultConfig exe & setUser "user")
        Module2.printFromModule2 "Hello"
    runAt
        (defaultConfig remoteExe
            & setRemote localhost
            & setUser "cw-portal")
        Module2.printFromModule2 "World"
$(rpcify 'top')
