module Main (main) where

--------------------------------------------------------------------------------
-- Imports
--------------------------------------------------------------------------------

import Data.Function ((&))
import Data.Maybe (fromJust, fromMaybe)
import Streamly.Unicode.String (str)
import System.FilePath ((</>))

import qualified Streamly.Internal.Data.Parser as Parser
import qualified Streamly.Internal.Data.Stream as Stream
import qualified Streamly.FileSystem.FileIO as File
import qualified Streamly.Internal.FileSystem.Path as Path
import qualified Streamly.Data.Fold as Fold
import qualified Streamly.Unicode.Stream as Unicode


import Data.Text (Text)
import qualified Data.Text.IO as T

import Prettyprinter
import Prettyprinter.Render.Text
import Data.Aeson

import System.Environment (getArgs)
import qualified Data.Aeson.KeyMap as KM
import qualified Data.Aeson.Key as KM

import qualified Streamly.Coreutils.Ls as Ls
import Streamly.Coreutils.Common (Switch(..))

--------------------------------------------------------------------------------
-- Types
--------------------------------------------------------------------------------

data Config =
    Config
        { distBuildDir :: String
        , sourceDirectories :: [String]
        , rpcModulePath :: String
        , rpcModuleName :: String
        }

data RpcModule =
    RpcModule
        { moduleName :: String
        , functionList :: [String]
        }
    deriving (Show)

parseConfigFromValue :: Value -> Config
parseConfigFromValue (Object obj) =
    fromMaybe (error "Unable to parse the config file") $ do
        Config
            <$> lookupVal "distBuildDir" obj
            <*> lookupVal "sourceDirectories" obj
            <*> lookupVal "rpcModulePath" obj
            <*> lookupVal "rpcModuleName" obj

    where

    fromJSON' a =
        case fromJSON a of
            Error err -> error err
            Success val -> val
    lookupVal k v = fmap fromJSON' (KM.lookup (KM.fromString k) v)
parseConfigFromValue _ = error "Expected an object"

slashToDot :: String -> String
slashToDot = map f
    where
    f '/' = '.'
    f x = x

--------------------------------------------------------------------------------
-- Get module listing
--------------------------------------------------------------------------------

getRpcModuleList :: Config -> IO [RpcModule]
getRpcModuleList Config{..} = do
    Stream.fromList sourceDirectories
        & fmap (\x -> distBuildDir </> x)
        & Stream.concatMap exploreDir
        & Stream.toList

    where

    isThHsFile fp =
        let len = length fp
            thHsSuffix = drop (len - 6) fp
            dynThHsSuffix = drop (len - 10) fp
         in thHsSuffix == ".th.hs" && dynThHsSuffix /= ".dyn.th.hs"

    -- XXX Use Path instead of String
    exploreDir dir =
        Ls.ls (Ls.recursive On) (fromJust $ Path.fromString dir)
            & Stream.catRights
            & fmap Path.toString
            & Stream.filter isThHsFile
            & Stream.mapM (parseTargetFile dir)

parseTargetFile :: FilePath -> FilePath -> IO RpcModule
parseTargetFile dirPrefix fp0 = do
    fp <- Path.fromString fp0
    fList <-
        File.read fp
            & Unicode.decodeUtf8'
            & Stream.foldMany (Fold.takeEndBy_ (== '\n') Fold.toList)
            & Stream.mapM (Stream.parse pragmaParser . Stream.fromList)
            & Stream.catRights
            & Stream.ordNub
            & Stream.toList
    pure $ RpcModule moduleName fList

    where

    moduleName =
        let lenPrefix = length dirPrefix
            val = drop (lenPrefix + 1) fp0
            lenVal = length val
         in slashToDot $ take (lenVal - 6) val

    -- {-# ANN functionName "RPC" #-}
    pragmaParser =
        Parser.listEq "{-# ANN "
            *> Parser.fromFold (Fold.takeEndBy_ (== ' ') Fold.toList)
            <* Parser.listEq "\"RPC\" #-}"

--------------------------------------------------------------------------------
-- Utils
--------------------------------------------------------------------------------

docWarning :: Doc ann
docWarning =
    pretty
        "-- Warning: This file is generated via CLI. Do not edit this \
        \directly."


docDivider :: Doc ann
docDivider = pretty $ replicate 80 '-'

docSectionComment :: String -> [Doc ann]
docSectionComment val =
    [ emptyDoc
    , docDivider
    , pretty $ "-- " ++ val
    , docDivider
    , emptyDoc
    ]

--------------------------------------------------------------------------------
-- Generation
--------------------------------------------------------------------------------

generateRpcModule :: Config -> IO Text
generateRpcModule conf@Config{..} = do
    rpcModuleList <- getRpcModuleList conf
    let docList = map getDoc $ zip ([0..] :: [Int]) rpcModuleList
    pure
        $ renderStrict $ layoutPretty defaultLayoutOptions $ vsep
        $ [docWarning, emptyDoc]
       ++ [docModuleDef]
       ++ docSectionComment "Imports"
       ++ [docImportList docList]
       ++ docSectionComment "Main"
       ++ docMainDef docList
       ++ [emptyDoc]


    where

    docModuleDef = pretty [str|module #{rpcModuleName} (rpcMap) where|]

    getDoc (i, RpcModule{..}) =
        let iStr = show i
            mkEvalDoc x = [str|evaluator Module#{iStr}.#{x}|]
         in ( pretty [str|import qualified #{moduleName} as Module#{iStr}|]
            , map mkEvalDoc functionList
            )

    docImportList docList =
        vsep
            $ pretty "import Simple.RPC.Types"
            : map fst docList
    docEvalList docList =
        let evalList = concat $ map snd docList
            pre = "[ " : replicate (length evalList - 1) ", "
         in vsep
                [ vsep $ map pretty $ zipWith (++) pre evalList
                , pretty "]"
                ]

    docMainDef docList =
        [ pretty "rpcMap :: RpcMap IntermediateRep"
        , pretty "rpcMap = createRpcMap"
        , indent 4 $ docEvalList docList
        ]


--------------------------------------------------------------------------------
-- Cabal
--------------------------------------------------------------------------------

main :: IO ()
main = do
    args <- getArgs
    case args of
        [configPath] -> do
            mvalue <- decodeFileStrict' configPath
            let value = fromMaybe (error "Improper JSON") mvalue
            let conf = parseConfigFromValue value
            T.writeFile (rpcModulePath conf) =<< generateRpcModule conf
        _ -> error "Exactly one argument is accepted"
