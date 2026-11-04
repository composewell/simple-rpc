{-# LANGUAGE QuasiQuotes #-}

module Simple.RPC.Internal.TH
    ( rpcExport
    , rpcify
    ) where

--------------------------------------------------------------------------------
-- Imports
--------------------------------------------------------------------------------

import Language.Haskell.TH
import Simple.RPC.Internal.Types

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

-- | @rpcExport origName rpcName@, creates a toplevel value definition with
-- the name @rpcName@, of type 'RpcSymbol' representing an RPC endpoint.
--
-- See the details under the docs of 'RpcSymbol'.
rpcExport :: String -> String -> Q [Dec]
rpcExport oldFuncName newFuncName = do
    modName <- loc_module <$> location

    let endpoint = newFuncName
        endpointWithMod = modName ++ "." ++ endpoint
        n_endpoint = mkName endpoint
        appEArgs = foldl appE

    -- Resolve the name, a name made by mkName would be captured by a local
    -- variable of the same name in the generated code.
    mRaw <- lookupValueName oldFuncName
    n_raw <-
        case mRaw of
            Just n -> pure n
            Nothing -> fail $ "rpcExport: not in scope: " ++ oldFuncName
    typ <- getFunctionType n_raw

    let numArgs = getNumArgs typ
        enumArgs = [0..(numArgs - 1)]
    n_runner <- newName "runner"
    n_val <- newName "val"
    n_args <- newName "args"
    argNames <- mapM (\i -> newName ('x' : show i)) enumArgs
    let patArgs = map varP argNames
        varArgs = map varE argNames

    -- Decode all the arguments before calling the function, so that an
    -- invalid input raises an exception before any effect of the function.
    let intE i = litE (integerL (fromIntegral i))
        bindArg i n = bindS (varP n) [|irArg $(varE n_args) $(intE i)|]
        argsPat = if numArgs == 0 then wildP else varP n_args
        evalExp =
            lamE [varP n_val]
                $ doE
                $ bindS argsPat [|irArgs $(intE numArgs) $(varE n_val)|]
                    : zipWith bindArg enumArgs argNames
                    ++ [ noBindS
                            [|toIntermediateRep
                                <$> $(appEArgs (varE n_raw) varArgs)|]
                       ]

    let toIntermediateRepTH k = [|toIntermediateRep $(k)|]
        encodedInput = [| createArr $(listE (map toIntermediateRepTH varArgs))|]

        runningExp =
            lamE
                (varP n_runner:patArgs)
                [|$(varE n_runner)
                     $(stringE endpointWithMod) $(encodedInput)
                     >>= resultFromIR|]

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
