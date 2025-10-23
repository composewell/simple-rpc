{-# LANGUAGE QuasiQuotes #-}

module Simple.RPC.Internal.TH
    ( rpcExport
    , rpcify
    ) where

--------------------------------------------------------------------------------
-- Imports
--------------------------------------------------------------------------------

import Language.Haskell.TH
import Simple.RPC.Types

--------------------------------------------------------------------------------
-- Functions
--------------------------------------------------------------------------------

getFunctionType :: Name -> Q Type
getFunctionType _func = do
    info <- reify _func
    case info of
        VarI _ typ_ _ -> pure typ_
        _ -> error "Improper type"

breakTypeOnArrow :: Type -> [Type]
breakTypeOnArrow (AppT (AppT ArrowT l) r) = l  : breakTypeOnArrow r
breakTypeOnArrow t = [t]

getNumArgs :: Type -> Int
getNumArgs typ = length (breakTypeOnArrow typ) - 1

-- | @rpcExport origName wrapperName@, creates an RPC endpoint function
-- @wrapperName@ which is a RPC enabling wrapper for the @origName@ function.
--
-- The RPC wrapper facilitates deserialization of the input arguments and
-- serialization of the output of the function.
rpcExport :: String -> String -> Q [Dec]
rpcExport oldFuncName newFuncName = do
    modName <- loc_module <$> location

    let endpoint = newFuncName
        endpointWithMod = modName ++ "." ++ endpoint
        n_raw = mkName oldFuncName
        n_endpoint = mkName endpoint
        n_runner = mkName "runner"
        n_val = mkName "val"
        appEArgs = foldl appE

    typ <- getFunctionType n_raw

    let numArgs = getNumArgs typ
        enumArgs = [0..(numArgs - 1)]
        strArgs = ('x':) . show <$> enumArgs
        patArgs = varP . mkName <$> strArgs
        varArgs = varE . mkName <$> strArgs
        arrIndexTH i =
            [|arrIndex $(varE n_val) $(litE (integerL (fromIntegral i)))|]
        parsedArgs = map arrIndexTH enumArgs


    let evalExp1 =
            lamE
                [(varP n_val)]
                [|toIntermediateRep <$> $(appEArgs (varE n_raw) parsedArgs)|]
        evalExp0 =
            [|\x -> ensureNullArray x (toIntermediateRep <$> $(varE n_raw))|]
        evalExp =
            if numArgs == 0
            then evalExp0
            else evalExp1

    let toIntermediateRepTH k = [|toIntermediateRep $(k)|]
        encodedInput = [| createArr $(listE (map toIntermediateRepTH varArgs))|]

        runningExp =
            lamE
                (varP n_runner:patArgs)
                [|fromIntermediateRep
                     <$> $(varE n_runner)
                             $(stringE endpointWithMod) $(encodedInput)|]

    let rpcSymbolExp =
            [|RpcSymbol
                  $(varE n_raw) $(runningExp)
                  (RpcEval $(stringE endpointWithMod) $(evalExp))|]

    prag <- pragAnnD (ValueAnnotation n_endpoint) [|"RPC"|]
    sig <- sigD n_endpoint [t|RpcSymbol IntermediateRep $(pure typ)|]
    def <- funD n_endpoint [ clause [] (normalB rpcSymbolExp) [] ]
    pure [prag, sig, def]

-- An opinionated rpc export

-- | The argument is the name of a function, the name must end with a single
-- quote symbol. @rpcify@ generates an RPC wrapper for that function, the name
-- of the wrapper is the original name without the single quote.
rpcify :: Name -> Q [Dec]
rpcify rawFuncName =
    let raw = nameBase rawFuncName
     in if last raw == '\''
        then rpcExport raw (init raw)
        else error "rpcify only works on function that ends with '"
