-- Warning: This file is generated via CLI. Do not edit this directly.

module Main (main) where

--------------------------------------------------------------------------------
-- Imports
--------------------------------------------------------------------------------

import Simple.RPC.Types
import Simple.RPC.Server
import qualified Module4 as Module0
import qualified Module3 as Module1
import qualified Module2 as Module2
import qualified Module1 as Module3

--------------------------------------------------------------------------------
-- Main
--------------------------------------------------------------------------------

main :: IO ()
main =
    mainWith "0.0.0" $ createRpcMap
        [ evaluator Module0.top
        , evaluator Module1.helloWorld
        , evaluator Module1.bottom
        , evaluator Module2.printFromModule2
        , evaluator Module3.printFromModule1
        ]
