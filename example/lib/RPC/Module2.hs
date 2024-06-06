-- Warning: This file is generated via CLI. Do not edit this directly.

module RPC.Module2
    ( printFromModule2
    ) where

--------------------------------------------------------------------------------
-- Imports
--------------------------------------------------------------------------------

import qualified Module2 as Lib
import Simple.RPC.Server

--------------------------------------------------------------------------------
-- RPC exports
--------------------------------------------------------------------------------

$(rpcExport "Lib.printFromModule2" "printFromModule2")
