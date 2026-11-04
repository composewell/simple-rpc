-- | Make RPC calls to Haskell functions in an executable on a local or a
-- remote host. This module re-exports "Simple.RPC.Client",
-- "Simple.RPC.Server" and "Simple.RPC.Common".
--
module Simple.RPC
(
    module  Simple.RPC.Client
  , module  Simple.RPC.Server
  , module  Simple.RPC.Common
)

where

import Simple.RPC.Client
import Simple.RPC.Server
import Simple.RPC.Common
