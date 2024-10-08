{-# LANGUAGE QuasiQuotes #-}

module Simple.RPC.TH
    ( rpcExport
    , rpcExportLenient
    ) where

--------------------------------------------------------------------------------
-- Imports
--------------------------------------------------------------------------------

import Language.Haskell.TH
import Simple.RPC.Types
import Data.Aeson

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

isRpcApplicable :: Type -> Q Bool
isRpcApplicable typ = do
    let components = breakTypeOnArrow typ
        args = init components
    case last components of
        (AppT (ConT nm) result) ->  do
            a <- fmap and $ mapM (isInstance ''FromJSON) $ map (:[]) args
            b <- isInstance ''ToJSON [result]
            let c = nm == ''IO
            pure (a && b && c)
        _ -> pure False

rpcExportLenient :: String -> String -> Q [Dec]
rpcExportLenient oldFuncName newFuncName = do
    let endpoint = newFuncName
        n_raw = mkName oldFuncName
        n_endpoint = mkName endpoint
    typ <- getFunctionType n_raw
    isApplicable <- isRpcApplicable typ
    if isApplicable
    then rpcExport oldFuncName newFuncName
    else do
        sig <- sigD n_endpoint (pure typ)
        def <- funD n_endpoint [ clause [] (normalB (varE n_raw)) [] ]
        pure [sig, def]

rpcExport :: String -> String -> Q [Dec]
rpcExport oldFuncName newFuncName = do
    modName <- loc_module <$> location

    let endpoint = newFuncName
        endpointWithMod = modName ++ "." ++ endpoint
        n_raw = mkName oldFuncName
        n_endpoint = mkName endpoint
        n_runner = mkName "runner"
        n_val = mkName "val"
        appEArgs f xs = foldl appE f xs

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
