module Module1
    ( printFromModule1
    ) where

printFromModule1 :: Int -> String -> Bool -> IO (Int, String, Bool)
printFromModule1 val1 val2 val3 = do
    putStrLn "From Module1,"
    print val1
    putStrLn val2
    print val3
    pure (val1, val2, val3)
