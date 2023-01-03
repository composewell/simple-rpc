{-# LANGUAGE QuasiQuotes #-}

module Simple.RPC.Setup (rpcSuffixHandler) where

--------------------------------------------------------------------------------
-- Imports
--------------------------------------------------------------------------------

import Control.Monad (when)
import Data.Function ((&))
import Distribution.Simple.PreProcess
import Distribution.Simple.Utils
import Distribution.PackageDescription
import Distribution.Simple.LocalBuildInfo
import Streamly.Unicode.String (str)

import qualified Streamly.Data.Stream as Stream
import qualified Streamly.Data.Fold as Fold

import qualified GHC.Parser as GP
import qualified GHC.Parser.Lexer as GP
import qualified GHC.Data.EnumSet as GP
import qualified GHC.Types.SrcLoc as GP
import qualified GHC.Data.FastString as GP
import qualified GHC.Data.StringBuffer as GP
import qualified Language.Haskell.Syntax as GP
import qualified GHC.Hs as GP

--------------------------------------------------------------------------------
-- Parsers
--------------------------------------------------------------------------------

rpcLine :: String -> String
rpcLine fname = [str|$(rpcExportLenient "raw_#{fname}" "#{fname}")|]

processNumberedLines
    :: [String] -- Functions to exclude
    -> Maybe String -- Last processed function
    -> [Either Int Int] -- Line numbers on importance:
                        -- "Left i" is Type declaration
                        -- "Right i" is Function declaration
    -> [(Int, String)] -- Current file lines tagged with line nubers
    -> [String] -- Processed file lines
processNumberedLines _ (Just fname) [] ys = map snd ys ++ [rpcLine fname]
processNumberedLines _ Nothing [] ys = map snd ys
processNumberedLines exLst prev xxs@(ex:xs) ((z, ln):ys) =
    if either id id ex == z
    then
        let fname = takeWhile (/= ' ') ln in
        if fname `elem` exLst
        then case prev of
               Just pfname ->
                   let rpcifyL = rpcLine pfname in
                   rpcifyL: ln : processNumberedLines exLst Nothing xs ys
               Nothing -> ln : processNumberedLines exLst prev xs ys
        else
            case processLineWith ex prev fname ln of
                (next, newLs) -> newLs ++ processNumberedLines exLst next xs ys
    else ln : processNumberedLines exLst prev xxs ys

    where

    processLineWith (Right _) p fname l =
        case p of
            Nothing ->
                let newL = "raw_" ++ l
                 in (Just fname, [newL])
            Just pfname ->
                let newL = "raw_" ++ l
                    rpcifyL = rpcLine pfname
                 in
                   if pfname == fname
                   then (Just pfname, [newL])
                   else (Just fname, [rpcifyL, newL])
    processLineWith (Left _) p _ l =
        case p of
            Nothing ->
                let newL = "raw_" ++ l
                 in (Nothing, [newL])
            Just fname ->
                let newL = "raw_" ++ l
                    rpcifyL = rpcLine fname
                 in (Nothing, [rpcifyL, newL])
processNumberedLines _ _ xs [] =
    error $ "The following line numbers aren't processed but are important: "
          ++ show xs

process
    :: FilePath
    -> IO String
process file = do
      contents <- readFile file
      (fline, skey, sval) <-
          Stream.fromList contents
              & Stream.fold
                    ((,,)
                         <$> Fold.takeEndBy_ (== '\n') Fold.toList
                         <*> Fold.take 13 Fold.toList
                         <*> Fold.takeEndBy_ (== '\n') Fold.toList)
      if fline == "-- RPCIFY_MODULE"
      then if skey == "-- EXCLUDING:"
           then let exList = filter (not . null) (words sval)
                 in processWith file exList contents
           else processWith file [] contents
      else pure contents

processWith
    :: FilePath
    -> [String]
    -> String
    -> IO String
processWith file excludingList contents = do
      let lineList = lines contents
      let buffer = GP.stringToStringBuffer contents
      case GP.unP GP.parseModule (parseState buffer) of
          GP.POk _ val -> do
              linesOfImportance <-
                  Stream.fromList (GP.hsmodDecls (GP.unLoc val))
                      & fmap processLocatedDec
                      & Stream.catMaybes
                      & Stream.toList
              pure
                  $ unlines
                  $ processNumberedLines excludingList
                        Nothing linesOfImportance (zip [1..] lineList)
          GP.PFailed pst ->
              let failedLine = show $ GP.srcLocLine (GP.psRealLoc (GP.loc pst))
               in error [str|Parsing failed at: #{failedLine}|]

      where

      optionalizeDec dec =
          case dec of
              GP.SigD _ (GP.TypeSig _ _ _) -> Just . Left
              GP.ValD _ (GP.FunBind _ _ _ _) -> Just . Right
              _ -> const Nothing
      processLocatedDec (GP.L src dec) =
          case GP.srcSpanStart (GP.locA src) of
              GP.RealSrcLoc loc _ -> optionalizeDec dec (GP.srcLocLine loc)
              GP.UnhelpfulLoc _ -> Nothing
      location = GP.mkRealSrcLoc (GP.mkFastString file) 1 1
      opts =
          GP.mkParserOpts
              (GP.fromList [])
              (GP.fromList [minBound..maxBound])
              False
              False
              False
              False
      parseState buffer = GP.initParserState opts buffer location

--------------------------------------------------------------------------------
-- Cabal preprocessor
--------------------------------------------------------------------------------

rpcPreprocessor
    :: Bool
    -> BuildInfo -> LocalBuildInfo -> ComponentLocalBuildInfo -> PreProcessor
rpcPreprocessor isDebugMode _ _ _ =
    PreProcessor
        { platformIndependent = True
        , runPreProcessor =
              mkSimplePreProcessor $ \inFile outFile verbosity -> do
                  notice
                      verbosity
                      (inFile ++ " is being preprocessed to " ++ outFile)
                  val <- process inFile
                  when isDebugMode $ do
                      putStrLn $ replicate 80 '-'
                      putStrLn $ "-- " ++ inFile
                      putStrLn $ replicate 80 '-'
                      putStrLn $ unlines $ map (">> " ++) $ lines val
                  writeFile outFile val
        }

rpcSuffixHandler :: Bool -> PPSuffixHandler
rpcSuffixHandler isDebugMode = ("hs", rpcPreprocessor isDebugMode)
