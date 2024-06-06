-- Warning: This file is generated via CLI. Do not edit this directly.

module RPC.Module1
    ( printFromModule1
    ) where

--------------------------------------------------------------------------------
-- Imports
--------------------------------------------------------------------------------

import qualified Module1 as Lib
import Simple.RPC.Server

--------------------------------------------------------------------------------
-- RPC exports
--------------------------------------------------------------------------------

$(rpcExport "Lib.printFromModule1" "printFromModule1")
