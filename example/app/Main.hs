module Main (main) where

import Module0 (exeVersion)
import RpcMap (rpcMap)
import Simple.RPC (mainWith)

main :: IO ()
main = mainWith exeVersion rpcMap
