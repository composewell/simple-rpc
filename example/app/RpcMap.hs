-- Warning: This file is generated via CLI. Do not edit this directly.

module RpcMap (rpcMap) where

--------------------------------------------------------------------------------
-- Imports
--------------------------------------------------------------------------------

import Simple.RPC
import qualified Module3 as Module0
import qualified Module4 as Module1
import qualified Module2 as Module2
import qualified Module1 as Module3

--------------------------------------------------------------------------------
-- Main
--------------------------------------------------------------------------------

rpcMap :: RpcMap IntermediateRep
rpcMap = createRpcMap
    [ evaluator Module0.helloWorld
    , evaluator Module0.bottom
    , evaluator Module1.top
    , evaluator Module2.printFromModule2
    , evaluator Module3.printFromModule1
    ]
