module Main (main) where

--------------------------------------------------------------------------------
-- Imports
--------------------------------------------------------------------------------

import Data.Function ((&))
import Data.Maybe (fromMaybe)
import Streamly.Unicode.String (str)
import Data.Foldable (forM_)
import System.FilePath ((</>), (<.>), takeDirectory)
import Data.Functor.Identity (runIdentity)

import qualified Streamly.Data.Stream as Stream
import qualified Streamly.Data.Fold as Fold
import qualified Streamly.Internal.System.Command as Cmd

import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T

import Prettyprinter
import Prettyprinter.Render.Text
import Data.Aeson

import System.Environment (getArgs)
import qualified Data.Aeson.KeyMap as KM
import qualified Data.Aeson.Key as KM

--------------------------------------------------------------------------------
-- Types
--------------------------------------------------------------------------------

data RpcModule =
    RpcModule
        { moduleName :: String
        , functionList :: [String]
        }

data Config =
    Config
        { cabalFile :: String
        , libraryRelativeToCabal :: String
        , libraryName :: String
        , rpcModulePrefix :: String
        , serverDirectoryRelativeToCabal :: String
        , serverExecutableName :: String
        , serverConfigImportList :: [String]
        , serverVersion :: String
        , rpcModuleList :: [RpcModule]
        }

parseConfigFromValue :: Value -> Config
parseConfigFromValue (Object obj) =
    fromMaybe (error "Unable to parse the config file") $ do
        Config
            <$> lookupText "cabalFile" obj
            <*> lookupText "libraryRelativeToCabal" obj
            <*> lookupText "libraryName" obj
            <*> lookupText "rpcModulePrefix" obj
            <*> lookupText "serverDirectoryRelativeToCabal" obj
            <*> lookupText "serverExecutableName" obj
            <*> lookupText "serverConfigImportList" obj
            <*> lookupText "serverVersion" obj
            <*> fmap
                    parseRpcModuleList
                    (KM.lookup (KM.fromString "rpcModuleList") obj)

    where

    fromJSON' a =
        case fromJSON a of
            Error err -> error err
            Success val -> val
    lookupText k v = fmap fromJSON' (KM.lookup (KM.fromString k) v)
    parseRpcModuleList (Object rmlObj) =
        map
            (\(a, b) -> RpcModule (KM.toString a) (fromJSON' b))
            (KM.toList rmlObj)
    parseRpcModuleList _ = error "Expected an object for rpcModuleList"
parseConfigFromValue _ = error "Expected an object"

--------------------------------------------------------------------------------
-- Utils
--------------------------------------------------------------------------------

dotToSlash :: String -> String
dotToSlash = map f
    where
    f '.' = '/'
    f x = x

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

generateServer :: Config -> Text
generateServer Config{..} = do
    renderStrict $ layoutPretty defaultLayoutOptions $ vsep
        $ [docWarning, emptyDoc]
       ++ [docModuleDef]
       ++ docSectionComment "Imports"
       ++ [docImportList]
       ++ docSectionComment "Main"
       ++ docMainDef
       ++ [emptyDoc]


    where

    docModuleDef = pretty "module Main (main) where"

    getDoc (i, RpcModule{..}) =
        let rpcModuleName = rpcModulePrefix ++ "." ++ moduleName
            iStr = show i
            mkEvalDoc x = [str|evaluator Module#{iStr}.#{x}|]
         in ( pretty [str|import qualified #{rpcModuleName} as Module#{iStr}|]
            , map mkEvalDoc functionList
            )

    docList = map getDoc $ zip ([0..] :: [Int]) rpcModuleList

    docImportList =
        vsep $ pretty "import Simple.RPC.Server" : map fst docList
    docEvalList =
        let evalList = concat $ map snd docList
            pre = "[ " : replicate (length evalList - 1) ", "
         in vsep
                [ vsep $ map pretty $ zipWith (++) pre evalList
                , pretty "]"
                ]

    docMainDef =
        [ pretty "main :: IO ()"
        , pretty "main ="
        , indent 4 $ vsep
              [ pretty [str|mainWith "#{serverVersion}"|]
              , indent 4 $ docEvalList
              ]
        ]


generateModule :: Config -> RpcModule -> Text
generateModule Config{..} RpcModule{..} = do
    renderStrict $ layoutPretty defaultLayoutOptions $ vsep
        $ [docWarning, emptyDoc]
       ++ docModuleDef
       ++ docSectionComment "Imports"
       ++ docImportList
       ++ docSectionComment "RPC exports"
       ++ docBody
       ++ [emptyDoc]

    where

    rpcModuleName = rpcModulePrefix ++ "." ++ moduleName

    docExportList =
        let len = length functionList
            pre = "( " : replicate (len - 1) ", "
         in map pretty $ zipWith (++) pre functionList

    docModuleDef =
        [ pretty [str|module #{rpcModuleName}|]
        , indent 4 $ vsep docExportList
        , indent 4 $ pretty ") where"
        ]

    docImportList =
        [ pretty [str|import qualified #{moduleName} as Lib|]
        , pretty "import Simple.RPC.Server"
        ]

    makeRpcExportStatement fname =
        pretty [str|$(rpcExport "Lib.#{fname}" "#{fname}")|]

    docBody = map makeRpcExportStatement functionList

generateCabalClient :: Config -> Text
generateCabalClient Config{..} =
    renderStrict $ layoutPretty defaultLayoutOptions docCommonClient

    where

    mkRpcModuleName x = rpcModulePrefix ++ "." ++ x
    rpcModuleListNames = map (mkRpcModuleName . moduleName) rpcModuleList
    pre = "  " : replicate (length rpcModuleListNames) ", "

    docRpcModuleList =
        vsep $ map pretty $ zipWith (++) pre rpcModuleListNames

    docCommonClient =
        vsep
            [ docWarningCabal "rpc-client"
            , emptyDoc
            , indent 4 $ pretty $ "exposed-modules:"
            , indent 6 $ docRpcModuleList
            , indent 4 $ pretty $ "build-depends: simple-rpc"
            , emptyDoc
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
            , indent 4 $ pretty [str|hs-source-dirs: #{serverDirectoryRelativeToCabal}|]
            , indent 4 $ pretty "default-language: Haskell2010"
            , emptyDoc
            ]

--------------------------------------------------------------------------------
-- Cabal
--------------------------------------------------------------------------------

data SrcLoc =
    SrcLoc
        { slRpcClient :: (Int, Int)
        , slRpcServer :: (Int, Int)
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
        Fold.teeWith (\a b -> SrcLoc <$> a <*> b)
            (Fold.teeWith (\a b -> (,) <$> a <*> b)
                (lineNumFold "-- <rpc-client>")
                (lineNumFold "-- </rpc-client>"))
            (Fold.teeWith (\a b -> (,) <$> a <*> b)
                (lineNumFold "-- <rpc-server>")
                (lineNumFold "-- </rpc-server>"))

slice :: Int -> Int -> [a] -> [a]
slice i j = take (j - i) . drop i

getModifiedCabalFile :: Config -> Text -> Either String Text
getModifiedCabalFile conf inp =
    case getAttributeSrcLoc inpLines of
        Left err -> Left err
        Right (SrcLoc (a, b) (i, j)) -> Right $
            T.unlines
                 $ take (a + 1) inpLines
                 ++ [generateCabalClient conf]
                 ++ slice b (i + 1) inpLines
                 ++ [generateCabalServer conf]
                 ++ drop j inpLines
    where
    inpLines = T.lines inp


modifyCabalFile :: Config -> IO ()
modifyCabalFile conf@Config{..} = do
    val <- T.readFile cabalFile
    case getModifiedCabalFile conf val of
        Left err -> error err
        Right newVal -> T.writeFile cabalFile newVal

makeRpcModules :: Config -> IO ()
makeRpcModules conf@Config{..} =
    forM_ rpcModuleList
        $ \rpcMod@(RpcModule{..}) -> do
              let rpcModuleName = rpcModulePrefix ++ "." ++ moduleName
                  libraryDirectory =
                      takeDirectory cabalFile </> libraryRelativeToCabal
                  dest = libraryDirectory </> dotToSlash rpcModuleName <.> ".hs"
                  destDirectory = takeDirectory dest
              Cmd.toStdout [str|mkdir -p #{destDirectory}|]
              T.writeFile dest $ generateModule conf rpcMod

makeRpcServer :: Config -> IO ()
makeRpcServer conf@(Config{..}) = do
    let serverDirectory =
            takeDirectory cabalFile </> serverDirectoryRelativeToCabal
        dest = serverDirectory </> "Main.hs"
    Cmd.toStdout [str|mkdir -p #{serverDirectory}|]
    T.writeFile dest $ generateServer conf

main :: IO ()
main = do
    args <- getArgs
    case args of
        [configPath] -> do
            mvalue <- decodeFileStrict' configPath
            let value = fromMaybe (error "Improper JSON") mvalue
            let config = parseConfigFromValue value
            modifyCabalFile config
            makeRpcModules config
            makeRpcServer config
        _ -> error "1 argument is required"
