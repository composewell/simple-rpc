module Module2
    ( printFromModule2
    ) where

import qualified Module1 as Module1

printFromModule2 :: String -> IO ()
printFromModule2 val = do
    putStrLn "From Module2,"
    val1 <- Module1.printFromModule1 10 val True
    print val1
