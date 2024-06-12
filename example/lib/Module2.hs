module Module2
    ( printFromModule2
    ) where

import Simple.RPC.Server
import qualified Module1 as Module1

raw_printFromModule2 :: String -> IO ()
raw_printFromModule2 val = do
    putStrLn "From Module2,"
    val1 <- run Module1.printFromModule1 10 val True
    print val1
$(rpcExport "raw_printFromModule2" "printFromModule2")
