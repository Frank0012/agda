-- | A very simple implementation of S-expressions that can be dumped to Text easily

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




agdoogle :: IO ()
agdoogle = do
    W.putStrLn "Name search or type search?" 
    W.putStrLn "[N] = name"
    W.putStrLn "[T] = type"
    --W.putStrLn "[F] = full name and type"
    selection <- Prelude.getLine

    if selection == "T" 
    then do W.putStrLn "Enter type to search"
            type' <- Prelude.getLine
            replaceType type'
            compile
            searchTerm <- TIO.readFile "SexpDatabase/searchTerm.agda-sexp"
            let result = recursiveTypeSearch getSexpDatabaseFiles searchTerm
            

            let groupedPositions = [(forEachDef positions (unsafePerformIO (TIOU.readTextFile ("AgdaDatabase/" ++ reverse (drop 5 (reverse path))))), 
                                    reverse (drop 5 (reverse path)), 
                                    getLineNumber (T.lines (unsafePerformIO (TIOU.readTextFile ("AgdaDatabase/" ++ reverse (drop 5 (reverse path)))))) (forEachDef positions (unsafePerformIO (TIOU.readTextFile ("AgdaDatabase/" ++ reverse (drop 5 (reverse path))))))) | 
                                    (path, positions) <- result]
            

            --mapM_ Prelude.putStr groupedPositions
            W.putStrLn "AGDOOGLE_SEARCH_RESULTS:"
            display groupedPositions
            

        
    else  
        do  W.putStrLn "Enter name"
            name  <- W.getLine
            
            let result = recursiveNameSearch getSexpDatabaseFiles name

            let groupedPositions = [((forEachDef positions (unsafePerformIO (TIOU.readTextFile ("AgdaDatabase/" ++ reverse (drop 5 (reverse path)))))), 
                                    reverse (drop 5 (reverse path)), 
                                    getLineNumber (T.lines (unsafePerformIO (TIOU.readTextFile ("AgdaDatabase/" ++ reverse (drop 5 (reverse path)))))) (forEachDef positions (unsafePerformIO (TIOU.readTextFile ("AgdaDatabase/" ++ reverse (drop 5 (reverse path))))))) | 
                                    (path, positions) <- result]
                    
            W.putStrLn "AGDOOGLE_SEARCH_RESULTS:"
            display groupedPositions
            

textToSexp :: DT.Text -> Sexp
textToSexp text = fst . head  $ parse sps (DT.unpack text) 


            
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




forEachDef :: Integer -> T.Text -> T.Text
forEachDef x str = (T.pack (getLineFromString x (T.unpack (prepareSource str))))

-- | All files in the Sexp database excluding searchTerm
getSexpDatabaseFiles :: [FilePath]
getSexpDatabaseFiles = filter (\x -> (x /= "searchTerm.agda-sexp")) (unsafePerformIO . listDirectory $ "SexpDatabase/")

getAgdaDatabaseFiles :: [FilePath]
getAgdaDatabaseFiles = filter (\x -> (reverse (take 4 (reverse x)) == "agda")) (unsafePerformIO . listDirectory $ "AgdaDatabase/")



getLineNumber :: [T.Text] -> T.Text -> Int
getLineNumber (x : file) line  = if (T.stripEnd line) == x then 1 else 1 + (getLineNumber file line )
getLineNumber _ line = 0



recursiveNameSearch :: [FilePath] -> T.Text -> [(FilePath, Integer)]
recursiveNameSearch filePaths name = do
    
    let fileTextsList = [ (file, (unsafePerformIO (TIO.readFile ("SexpDatabase/" ++ file)))) | file <- filePaths ]
    let result = [ ((fst file) , head (returnRange (Cons x))) | 
                 file <- fileTextsList,
                 reverse (drop 5 (reverse (fst file))) `elem` getAgdaDatabaseFiles, 
                 Cons x <- findName name $ textToSexp (snd file)]

    result


recursiveTypeSearch :: [FilePath] -> Text -> [(FilePath, Integer)]
recursiveTypeSearch filePaths searchTerm = do

    let fileTextsList = [ (file, (unsafePerformIO (TIO.readFile ("SexpDatabase/" ++ file)))) | file <- filePaths ]
    let result = [ ((fst file) , head (returnRange (Cons x))) | 
                 file <- fileTextsList,
                 -- Make sure file exists in Agda Database 
                 reverse (drop 5 (reverse (fst file))) `elem` getAgdaDatabaseFiles,
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



replaceType :: String -> IO ()
replaceType type' = do
  contents <- TIO.readFile "AgdaDatabase/searchTerm.agda"
  let fileLines = DT.lines contents
  let fileLinesWithImports = (createImports getImports) ++ filter (\x -> not (DT.isInfixOf (DT.pack "open import") x)) fileLines
  let modifiedContents = DT.unlines $ map replaceLine fileLinesWithImports
  withFile "AgdaDatabase/searchTerm.agda" WriteMode $ \handle -> do
    TIO.hPutStr handle modifiedContents
  where
    replaceLine line
      | DT.isInfixOf (DT.pack ":") line = DT.pack ("searchTerm : " ++ type')
      | otherwise = line


-- | Import all files in the Agda database into the search term database. Naive but covers most cases.
getImports :: [FilePath]
getImports = filter (\str -> str /= "searchTerm.agda") getAgdaDatabaseFiles

createImports :: [FilePath] -> [Text]
createImports [] = [DT.pack "open import Agda.Primitive"]
createImports (x : xs) = DT.pack ("open import " ++ (reverse (drop 5 (reverse x)))) : createImports xs




---- TODO: FIX THIS ----
compile :: IO ()
compile = callCommand "cd AgdaDatabase && agda --sexp --allow-unsolved-metas searchTerm.agda"


extractTypeFromSearch :: Sexp -> [Sexp]
extractTypeFromSearch (Cons mod) = concat [types | (Cons ((Atom ":definition") : ((Cons name) : ((Cons types) : functions))))  <- mod]
extractTypeFromSearch _ = [String "nothing found"]


-- | Given a function name and a sexp, return the top level definition clause matching that function name, wrapped in a list
findName :: T.Text -> Sexp -> [Sexp]
findName name (Cons mod) = do
    (Cons ((Atom ":definition") : ((Cons [nameAtom, Cons modulename, Cons defname]) : spss)))  <- mod
    (Atom something) <- defname
    guard (something == (':' `T.cons` name))
    return (Cons ((Atom ":definition") : ((Cons [nameAtom, Cons modulename, Cons defname]) : spss)))
findName name _ = [String "nothing found"]

-- | Given a sexp of a type signature, return the top level definition clause matching that type signature, wrapped in a list
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



    



