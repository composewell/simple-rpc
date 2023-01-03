-- RPCIFY_MODULE
module Module2
    ( printFromModule2
    ) where

import qualified Module1 as Module1
import Simple.RPC.Server

printFromModule2 :: String -> IO ()
printFromModule2 val = do
    putStrLn "From Module2,"
    val1 <- run Module1.printFromModule1 10 val True
    print val1
