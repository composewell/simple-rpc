module Main (main) where

--------------------------------------------------------------------------------
-- Imports
--------------------------------------------------------------------------------

import Data.Function ((&))
import Data.Maybe (fromMaybe)
import Streamly.Unicode.String (str)
import System.FilePath ((</>))
import Data.Functor.Identity (runIdentity)

import qualified Streamly.Internal.Data.Parser as Parser
import qualified Streamly.Internal.Data.Stream as Stream
import qualified Streamly.FileSystem.File as File
import qualified Streamly.Data.Fold as Fold
import qualified Streamly.Internal.System.Command as Cmd
import qualified Streamly.Unicode.Stream as Unicode


import Data.Text (Text)
import qualified Data.Text as T
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
        , cabalFile :: String
        , libraryName :: String
        , serverDirectory :: String
        , serverExecutableName :: String
        , serverConfigImportList :: [String]
        , serverVersion :: String
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
            <*> lookupVal "cabalFile" obj
            <*> lookupVal "libraryName" obj
            <*> lookupVal "serverDirectory" obj
            <*> lookupVal "serverExecutableName" obj
            <*> lookupVal "serverConfigImportList" obj
            <*> lookupVal "serverVersion" obj

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
         in drop (len - 6) fp == ".th.hs"

    exploreDir dir =
        Ls.ls (Ls.recursive On) dir
            & Stream.catRights
            & Stream.filter isThHsFile
            & Stream.mapM (parseTargetFile dir)

parseTargetFile :: FilePath -> FilePath -> IO RpcModule
parseTargetFile dirPrefix fp = do
    fList <-
        File.read fp
            & Unicode.decodeUtf8'
            & Stream.foldMany (Fold.takeEndBy_ (== '\n') Fold.toList)
            & Stream.mapM (Stream.parse pragmaParser . Stream.fromList)
            & Stream.catRights
            & Stream.nub
            & Stream.toList
    pure $ RpcModule moduleName fList

    where

    moduleName =
        let lenPrefix = length dirPrefix
            val = drop (lenPrefix + 1) fp
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

docWarningCabal :: String -> Doc ann
docWarningCabal val = pretty [str|#
-- Warning: The section between <#{val}> and </#{val}> is managed by the rpc
-- generator. Do not edit this by hand.#
|]


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

generateServer :: Config -> IO Text
generateServer conf@Config{..} = do
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

    docModuleDef = pretty "module Main (main) where"

    getDoc (i, RpcModule{..}) =
        let iStr = show i
            mkEvalDoc x = [str|evaluator Module#{iStr}.#{x}|]
         in ( pretty [str|import qualified #{moduleName} as Module#{iStr}|]
            , map mkEvalDoc functionList
            )

    docImportList docList =
        vsep $ pretty "import Simple.RPC.Server" : map fst docList
    docEvalList docList =
        let evalList = concat $ map snd docList
            pre = "[ " : replicate (length evalList - 1) ", "
         in vsep
                [ vsep $ map pretty $ zipWith (++) pre evalList
                , pretty "]"
                ]

    docMainDef docList =
        [ pretty "main :: IO ()"
        , pretty "main ="
        , indent 4 $ vsep
              [ pretty [str|mainWith "#{serverVersion}"|]
              , indent 4 $ docEvalList docList
              ]
        ]

generateCabalServer :: Config -> Text
generateCabalServer Config{..} =
    renderStrict $ layoutPretty defaultLayoutOptions docServer

    where

    preComma i = "  " : replicate i ", "
    zipCommas lst = zipWith (++) (preComma (length lst)) lst
    docServer =
        vsep
            [ docWarningCabal "rpc-server"
            , emptyDoc
            , pretty [str|executable #{serverExecutableName}|]
            , indent 4
                  $ vsep
                        [ pretty "import:"
                        , indent 2
                              $ vsep
                              $ map pretty $ zipCommas serverConfigImportList
                        ]
            , indent 4 $ pretty "main-is: Main.hs"
            , indent 4
                  $ vsep
                        [ pretty "build-depends:"
                        , indent 2
                              $ vsep
                              $ map pretty $ zipCommas
                              $ ["base", "simple-rpc", libraryName]
                        ]
            , indent 4 $ pretty [str|hs-source-dirs: #{serverDirectory}|]
            , indent 4 $ pretty "default-language: Haskell2010"
            , emptyDoc
            ]

--------------------------------------------------------------------------------
-- Cabal
--------------------------------------------------------------------------------

data SrcLoc =
    SrcLoc
        { slRpcServer :: (Int, Int)
        }
    deriving (Show)

getAttributeSrcLoc :: [Text] -> Either String SrcLoc
getAttributeSrcLoc inp =
    Stream.fromList inp
        & Stream.fold srclocFold
        & runIdentity

    where

    lineNumFold val =
        fmap
            (maybe (Left $ val ++ " not found") Right)
            (Fold.findIndex ((==) (T.pack val) . T.take (length val)))
    srclocFold =
        fmap SrcLoc <$>
            (Fold.teeWith (\a b -> (,) <$> a <*> b)
                (lineNumFold "-- <rpc-server>")
                (lineNumFold "-- </rpc-server>"))

getModifiedCabalFile :: Config -> Text -> Either String Text
getModifiedCabalFile conf inp =
    case getAttributeSrcLoc inpLines of
        Left err -> Left err
        Right (SrcLoc (a, b)) -> Right $
            T.unlines
                 $ take (a + 1) inpLines
                 ++ [generateCabalServer conf]
                 ++ drop b inpLines
    where
    inpLines = T.lines inp


modifyCabalFile :: Config -> IO ()
modifyCabalFile conf@Config{..} = do
    val <- T.readFile cabalFile
    case getModifiedCabalFile conf val of
        Left err -> error err
        Right newVal -> T.writeFile cabalFile newVal

makeRpcServer :: Config -> IO ()
makeRpcServer conf@(Config{..}) = do
    let dest = serverDirectory </> "Main.hs"
    Cmd.toStdout [str|mkdir -p #{serverDirectory}|]
    T.writeFile dest =<< generateServer conf

main :: IO ()
main = do
    args <- getArgs
    case args of
        [configPath] -> do
            mvalue <- decodeFileStrict' configPath
            let value = fromMaybe (error "Improper JSON") mvalue
            let config = parseConfigFromValue value
            modifyCabalFile config
            makeRpcServer config
        _ -> error "Exactly one argument is accepted"
