-- | Types and functions common to client and server.
--
module Simple.RPC.Common
    (
    -- * RPC Wrappers
      RpcSymbol (evaluator) -- XXX Rpc, remove evaluator
    , rpcExport -- XXX mkRpc
    , rpcName

    -- * RPC Serialization
    -- XXX Ideally, should not be needed, but used at one place for manually
    -- generating an RPC call command, serializing the arguments for supplying
    -- to the rpc executable on stdin.
    , IntermediateRep
    , toIntermediateRep
    , toBinStream

    -- * Deprecated
    , rpcify
    ) where

import Simple.RPC.Internal.TH (rpcExport, rpcify)
import Simple.RPC.Internal.Types
