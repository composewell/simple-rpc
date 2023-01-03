#!/usr/bin/env runhaskell

--------------------------------------------------------------------------------
-- Imports
--------------------------------------------------------------------------------

import Distribution.Simple
    (hookedPreProcessors, simpleUserHooks, defaultMainWithHooks)
import Distribution.Simple.PreProcess (knownSuffixHandlers)
import Simple.RPC.Setup (rpcSuffixHandler)

--------------------------------------------------------------------------------
-- Cabal preprocessor
--------------------------------------------------------------------------------

main :: IO ()
main =
    let hooks = simpleUserHooks
     in defaultMainWithHooks hooks
            { hookedPreProcessors =
                  (rpcSuffixHandler True):knownSuffixHandlers
            }
