-- Warning: This file is generated via CLI. Do not edit this directly.

module Main (main) where

--------------------------------------------------------------------------------
-- Imports
--------------------------------------------------------------------------------

import Simple.RPC.Server
import qualified RPC.Module1 as Module0
import qualified RPC.Module2 as Module1
import qualified RPC.Module3 as Module2
import qualified RPC.Module4 as Module3

--------------------------------------------------------------------------------
-- Main
--------------------------------------------------------------------------------

main :: IO ()
main =
    mainWith "0.0.0"
        [ evaluator Module0.printFromModule1
        , evaluator Module1.printFromModule2
        , evaluator Module2.helloWorld
        , evaluator Module2.bottom
        , evaluator Module3.top
        ]
