-- Warning: This file is generated via CLI. Do not edit this directly.

module RPC.Module4
    ( top
    ) where

--------------------------------------------------------------------------------
-- Imports
--------------------------------------------------------------------------------

import qualified Module4 as Lib
import Simple.RPC.Server

--------------------------------------------------------------------------------
-- RPC exports
--------------------------------------------------------------------------------

$(rpcExport "Lib.top" "top")
