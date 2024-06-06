-- Warning: This file is generated via CLI. Do not edit this directly.

module RPC.Module3
    ( helloWorld
    , bottom
    ) where

--------------------------------------------------------------------------------
-- Imports
--------------------------------------------------------------------------------

import qualified Module3 as Lib
import Simple.RPC.Server

--------------------------------------------------------------------------------
-- RPC exports
--------------------------------------------------------------------------------

$(rpcExport "Lib.helloWorld" "helloWorld")
$(rpcExport "Lib.bottom" "bottom")
