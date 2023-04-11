{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}

module Agdoogle where

import Parsing
import Debug.Trace
import Data.Word
import qualified Data.Text.Lazy as T
import qualified Data.Text as DT
import Data.Text.Lazy.IO as W
import qualified Data.Text.IO as TIO
import System.Process
import System.Info
import Data.Char
import Main.Utf8
import qualified System.IO.Utf8 as Utf8
import Control.Monad (guard)
import System.Directory
import System.IO
import Data.Text (Text, index, count)
import GHC.Float (fromRat'')
import System.IO.Unsafe
import qualified Agda.Utils.IO.UTF8 as TIOU
import qualified System.FilePath as SF
import qualified System.Directory.Recursive as SDR


agdoogle :: IO ()
agdoogle = do
    W.putStrLn "Name search or type search?" 
    W.putStrLn "[N] = name"
    W.putStrLn "[T] = type"
    selection <- Prelude.getLine
    if selection == "T" 
    then do W.putStrLn "Enter type to search"
            type' <- Prelude.getLine
            replaceType type'
            compile
            searchTerm <- TIO.readFile "SexpDatabase/searchTerm.agda-sexp"
            let matches = recursiveTypeSearch getSexpDatabaseFiles searchTerm
            let result = searchResult matches
            W.putStrLn "AGDOOGLE_SEARCH_RESULTS:"
            if null result then W.putStrLn "NO MATCHING TYPES" else display result
    else  
        do  W.putStrLn "Enter name"
            name  <- W.getLine
            let matches = recursiveNameSearch getSexpDatabaseFiles name
            let result = searchResult matches         
            W.putStrLn "AGDOOGLE_SEARCH_RESULTS:"
            if null result then W.putStrLn "NO MATCHING NAMES" else display result

-- | We format the result as (textFromFile, fileName, lineNumber)
searchResult :: [(String, Integer)] -> [(T.Text, String, Int)]
searchResult lst = [(lineFromFile, 
                    fromSexpToAgda path, 
                    getLineNumber (T.lines file) lineFromFile) | 
                    (path, positions) <- lst,
                    let file = unsafePerformIO (TIOU.readTextFile . fromSexpToAgda $ path),
                    let lineFromFile = forEachDef positions file]

-- | Convert the text files back to Sexp (parse)
textToSexp :: DT.Text -> Sexp
textToSexp text = fst . head  $ parse sps (DT.unpack text) 

-- | Create an agda file name from the Sexp one
fromSexpToAgda :: FilePath -> FilePath
fromSexpToAgda fp = head $ filter (\x -> cleanName (SF.dropExtension fp) == normalisePath (SF.makeRelative "AgdaDatabase" (SF.dropExtensions x))) getAgdaDatabaseFiles
    
-- | Turn Sexp namespaced names back to paths
cleanName :: FilePath -> FilePath
cleanName f = map (\x -> if x == '.' then '/' else x) f

-- | Display the results           
display :: [(T.Text, [Char], Int)] -> IO ()
display [] = Prelude.putStr []
display ((line, file, lineNum) : xs) =  do W.putStr "DEFINITION (" 
                                           W.putStr line
                                           W.putStr "), "
                                           W.putStr "FILE ("
                                           Prelude.putStr file
                                           W.putStr "), "
                                           W.putStr "LINE_NUMBER ("
                                           Prelude.putStr (show lineNum)
                                           W.putStrLn ") "
                                           display xs

-- | Let Agdoogle know that \\ == / for a path
normalisePath :: FilePath -> FilePath
normalisePath path = map (\inp -> if inp == '\\' then '/' else inp) path

containsPath :: FilePath -> [FilePath] -> Bool
containsPath x y = (normalisePath x) `elem` (map normalisePath y)

-- | Getting the line at the number specified from the source file
forEachDef :: Integer -> T.Text -> T.Text
forEachDef x str = (T.pack (getLineFromString x (T.unpack (prepareSource str))))

-- | All files in the Sexp database excluding searchTerm
getSexpDatabaseFiles :: [FilePath]
getSexpDatabaseFiles = filter (\x -> (x /= "searchTerm.agda-sexp")) (unsafePerformIO . listDirectory $ "SexpDatabase/")

getAgdaDatabaseFiles :: [FilePath]
getAgdaDatabaseFiles = filter (\x -> (SF.takeExtensions x == ".agda"
                                   || SF.takeExtensions x == ".lagda"
                                   || SF.takeExtensions x == ".lagda.md"
                                   || SF.takeExtensions x == ".lagda.tex"
                                   || SF.takeExtensions x == ".lagda.rst")) 
                              (unsafePerformIO . SDR.getDirRecursive $ "AgdaDatabase/")

-- | Only called when we know there definitely is a matching definition in the file
getLineNumber :: [T.Text] -> T.Text -> Int
getLineNumber (x : file) line  = if (T.stripEnd line) == x then 1 else 1 + (getLineNumber file line )
getLineNumber _ line = 0

recursiveNameSearch :: [FilePath] -> T.Text -> [(FilePath, Integer)]
recursiveNameSearch filePaths name = do
    let fileTextsList = [ (file, (unsafePerformIO (TIO.readFile ("SexpDatabase/" ++ file)))) | file <- filePaths ]
    let result = [ ((fst file) , head (returnRange (Cons x))) | 
                 file <- fileTextsList,
                 -- Make sure file exists in Agda Database
                 containsPath (cleanName . SF.dropExtension $ (fst file)) (map ((SF.makeRelative "AgdaDatabase") . SF.dropExtensions) getAgdaDatabaseFiles), 
                 Cons x <- findName name $ textToSexp (snd file)]
    result

recursiveTypeSearch :: [FilePath] -> Text -> [(FilePath, Integer)]
recursiveTypeSearch filePaths searchTerm = do
    let fileTextsList = [ (file, (unsafePerformIO (TIO.readFile ("SexpDatabase/" ++ file)))) | file <- filePaths ]
    let result = [ ((fst file) , head (returnRange (Cons x))) | 
                 file <- fileTextsList,
                 -- Make sure file exists in Agda Database 
                 containsPath (cleanName . SF.dropExtension $ (fst file)) (map ((SF.makeRelative "AgdaDatabase") . SF.dropExtensions) getAgdaDatabaseFiles),
                 Cons x <- findType (extractTypeFromSearch (textToSexp searchTerm)) $ textToSexp (snd file)
                ]
    result

-- | Return the line number which the given character postition falls on
getLineFromString :: Integer -> String -> String
getLineFromString num str = reverse (dropNextLine (reverse (take (fromIntegral num) str))) ++ (dropNextLine (drop (fromIntegral num) str))

-- | Windows line numbers are slightly different to UNIX based systems - cleaning here if Windows
prepareSource :: T.Text -> T.Text
prepareSource str = if os == "mingw32" then T.intercalate (T.pack "\n") cleanedText else str
    where
        splitText = [ x | x <- (T.splitOn (T.pack "\n") str) ]
        cleanedText = map (\x -> if x /= (T.pack "") && x /= (T.pack "") then T.append (x) (T.pack " ") else T.append (x) (T.pack "")) splitText

dropNextLine :: String -> String
dropNextLine [] = []
dropNextLine ('\n' : str) = ""
dropNextLine (s:str) = s : dropNextLine str

-- | Replace the type of searchTerm in searchTerm.agda
replaceType :: String -> IO ()
replaceType type' = do
  fileText <- TIO.readFile "AgdaDatabase/searchTerm.agda"
  let fileLines = DT.lines fileText
  let fileLinesWithImports = (createImports getImports) ++ filter (\x -> not (DT.isInfixOf (DT.pack "open import") x)) fileLines
  let newFileLines = map (appendSignature type') fileLinesWithImports
  let newFileText = DT.unlines newFileLines
  let write = \x -> TIO.hPutStr x newFileText
  withFile "AgdaDatabase/searchTerm.agda" WriteMode write

appendSignature :: String -> DT.Text -> DT.Text
appendSignature type' line = if DT.isInfixOf (DT.pack "postulate") line then
                                                                        DT.pack ("postulate searchTerm : " ++ type')
                                                                        else
                                                                        line

-- | Import all files in the Agda database into the search term database. Naive but covers most cases.
getImports :: [FilePath]
getImports = filter (\str -> str /= "AgdaDatabase/searchTerm.agda") getAgdaDatabaseFiles

createImports :: [FilePath] -> [Text]
createImports [] = [DT.pack "open import Agda.Primitive"]
createImports (x : xs) = DT.pack ("open import " ++ (convertToImportName x)) : createImports xs 

-- | Turn the paths into import names
convertToImportName :: FilePath -> String
convertToImportName name = map (\x -> if SF.isPathSeparator x then '.' else x) 
                               (SF.makeRelative "AgdaDatabase" (SF.dropExtensions name))

-- | Generate S-expression for searchTerm.agda
compile :: IO ()
compile = callCommand "cd AgdaDatabase && agda --sexp searchTerm.agda"

extractTypeFromSearch :: Sexp -> [Sexp]
extractTypeFromSearch (Cons mod) = concat [types | (Cons ((Atom ":definition") : ((Cons name) : ((Cons types) : functions))))  <- mod]
extractTypeFromSearch _ = [String "nothing found"]

-- | Given a function name and a sexp, return the top level definition clause matching that function name
findName :: T.Text -> Sexp -> [Sexp]
findName name (Cons mod) = do
    (Cons ((Atom ":definition") : ((Cons names) : spss)))  <- mod
    Cons nameForFunc <- [head (reverse names)]
    (Atom something) <- nameForFunc
    guard (something == (':' `T.cons` name))
    
    return (Cons ((Atom ":definition") : ((Cons names) : spss)))   
findName name _ = [String "nothing found"]

-- | Given a sexp of a type signature, return the top level definition clause matching that type signature
findType :: [Sexp] -> Sexp -> [Sexp]
findType type' (Cons mod) =
    [Cons ((Atom ":definition") : ((Cons name) : ((Cons types) : defs))) | 
    (Cons ((Atom ":definition") : ((Cons name) : ((Cons types) : defs)))) <- mod, 
    removeRangeFromType types == removeRangeFromType type'] 
findType type' _ = [String "nothing found"]

removeRangeFromType :: [Sexp] -> [Sexp]
removeRangeFromType [] = []
removeRangeFromType (Cons (Atom ":position" : (Integer n : (Integer x : (Integer q : [])))) : more) = (Atom ":position") : (removeRangeFromType more)
removeRangeFromType ((Cons s) : more) = (Cons (removeRangeFromType s)) : removeRangeFromType more
removeRangeFromType (s:str) = s : removeRangeFromType str

returnRange :: Sexp -> [Integer]
returnRange (Cons [definitionAtom , Cons names, types, (Cons datas)]) = do
    return (head outerRange)
    where
        outerRange = [ pospos | Cons [Atom fn, 
                                        Atom n, 
                                        Cons [Atom rng, 
                                        range, 
                                        Cons [Atom intervalwithoutfile, 
                                        Cons [Atom interval, 
                                        Cons [Atom pos, Integer pospos, Integer line, Integer col], 
                                        Cons endpos], 
                                        extra]]] <- [head (reverse names)]]



    



