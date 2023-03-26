-- | A very simple implementation of S-expressions that can be dumped to Text easily

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}

module Agdoogle where

import Debug.Trace
import Data.Word

import qualified Data.Text.Lazy as T
import qualified Data.Text as DT
import Data.Text.Lazy.IO as W

import qualified Data.Text.IO as TIO



import System.Process
import System.Info

import Control.Applicative ( Alternative(many, (<|>), empty) )
import qualified Control.Applicative as CA
import Data.Char


import Data.Char (isDigit, digitToInt)

import Main.Utf8
import qualified System.IO.Utf8 as Utf8

import Control.Monad (guard)


import System.Directory


import System.IO
import Data.Text (Text, index, count)
import GHC.Float (fromRat'')
import System.IO.Unsafe


---------------------------------------------------------------------------
---------------------------------------------------------------------------
----------------------------TO DO : UTF8 OUTPUT----------------------------
---------------------------------------------------------------------------
---------------------------------------------------------------------------


textToSexp :: DT.Text -> Sexp
textToSexp text = fst . head  $ parse sps (DT.unpack text) 

agdoogle :: IO () 
agdoogle = do ---withUtf8 $ do
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
        
            
            let groupedPositions = [(forEachDef positions (unsafePerformIO (TIO.readFile ("AgdaDatabase/" ++ reverse (drop 5 (reverse path))))), 
                                    reverse (drop 5 (reverse path)), 
                                    getLineNumber (DT.lines (unsafePerformIO (TIO.readFile ("AgdaDatabase/" ++ reverse (drop 5 (reverse path)))))) (forEachDef positions (unsafePerformIO (TIO.readFile ("AgdaDatabase/" ++ reverse (drop 5 (reverse path))))))) | 
                                    (path, positions) <- result]
            

            mapM_ print groupedPositions

        
    else (if selection == "N" then 
        do  W.putStrLn "Enter name"
            name  <- W.getLine
            
            let result = recursiveNameSearch getSexpDatabaseFiles name

            let groupedPositions = [((forEachDef positions (unsafePerformIO (TIO.readFile ("AgdaDatabase/" ++ reverse (drop 5 (reverse path)))))), 
                                    reverse (drop 5 (reverse path)), 
                                    getLineNumber (DT.lines (unsafePerformIO (TIO.readFile ("AgdaDatabase/" ++ reverse (drop 5 (reverse path)))))) (forEachDef positions (unsafePerformIO (TIO.readFile ("AgdaDatabase/" ++ reverse (drop 5 (reverse path))))))) | 
                                    (path, positions) <- result]
                    
            
            mapM_ print (groupedPositions)
            

        else do W.putStrLn "Enter name and type")


forEachDef :: Integer -> DT.Text -> Text
forEachDef x str = (DT.pack (getLineFromString x (DT.unpack (prepareSource str))))

getSexpDatabaseFiles :: [FilePath]
getSexpDatabaseFiles = unsafePerformIO . listDirectory $ "SexpDatabase/"

getAgdaDatabaseFiles :: [FilePath]
getAgdaDatabaseFiles = unsafePerformIO . listDirectory $ "AgdaDatabase/"


getLineNumber :: [Text] -> Text -> Int
getLineNumber (x : file) line  = if (DT.stripEnd line) == x then 1 else 1 + (getLineNumber file line )
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

-- Dealing with carriage returns for Windows
prepareSource :: DT.Text -> DT.Text
prepareSource str = DT.intercalate (DT.pack "\n") cleanedText    ---(if os == "mingw32" then cleanedText else splitText)
    where
        splitText = [ x | x <- (DT.splitOn (DT.pack "\n") str) ]
        cleanedText = map (\x -> if x /= (DT.pack "") && x /= (DT.pack "") then DT.append (x) (DT.pack " ") else DT.append (x) (DT.pack "")) splitText

dropNextLine :: String -> String
dropNextLine [] = []
dropNextLine ('\n' : str) = ""
dropNextLine (s:str) = s : dropNextLine str



replaceType :: String -> IO ()
replaceType type' = do
  contents <- TIO.readFile "AgdaDatabase/searchTerm.agda"
  let modifiedContents = DT.unlines $ map replaceLine (DT.lines contents)
  withFile "AgdaDatabase/searchTerm.agda" WriteMode $ \handle -> do         ------Utf8.withFile "AgdaDatabase/searchTerm.agda" WriteMode $ \handle -> do   -------------------
    TIO.hPutStr handle modifiedContents
  where
    replaceLine line
      | DT.isInfixOf (DT.pack ":") line = DT.pack ("searchTerm : " ++ type')
      | otherwise = line






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



    

data Sexp = Atom T.Text | String String | Integer Integer | Double Double | Cons [Sexp]
            deriving (Show, Eq)

constr :: String -> [Sexp] -> Sexp
constr head lst = Cons (Atom (':' `T.cons` (T.pack head)) : lst)

--toText :: Sexp -> T.Text
--toText (Atom x)   = x
--toText (Integer k) = T.pack $ show k
--toText (Double x) = T.pack $ show x
--toText (String s) = T.pack $ show s
--toText (Cons lst) = '(' `T.cons` (T.intercalate (T.singleton ' ') (map toText lst)) `T.snoc` ')'

toText :: Sexp -> T.Text
toText (Atom x)   = T.pack $ "Atom " ++ T.unpack x
toText (Integer k) = T.pack $ "Integer " ++ show k
toText (Double x) = T.pack $ "Double " ++ show x
toText (String s) = T.pack $ "String " ++ show s
toText (Cons lst) = '[' `T.cons` (T.intercalate (T.singleton ' ') (map toText lst)) `T.snoc` ']'





class Sexpable a where
    toSexp :: a -> Sexp

instance Sexpable Bool where
    toSexp False = constr "false" []
    toSexp True = constr "true" []

instance Sexpable Integer where
    toSexp k = Integer k

instance Sexpable Int where
    toSexp k = Integer (toInteger k)

instance Sexpable String where
    toSexp = String

instance Sexpable T.Text where
    toSexp t = String $ T.unpack t

instance Sexpable DT.Text where
    toSexp t = String $ DT.unpack t

instance Sexpable Double where
    toSexp = Double

instance Sexpable Word64 where
    toSexp w = Integer (toInteger w)



-- Much of the following code is credited to Graham Hutton
-- from Programming in Haskell second edition and his paper
-- Monadic Parser Combinators written with Erik Meijer
newtype Parser a = P (String -> [(a, String)])

instance Functor Parser where
    fmap g p = P (\inp -> case parse p inp of 
                    [(v, out)] -> [(g v, out)]
                    _          -> [])

instance Applicative Parser where
    pure v = P (\inp -> [(v, inp)])

    pg <*> px = P (\inp -> case parse pg inp of 
                    [(g, out)] -> parse (fmap g px) out
                    _          -> [])

instance Monad Parser where
    p >>= f = P (\inp -> case parse p inp of
                    [(v, out)] -> parse (f v) out
                    _          -> [])

instance Alternative Parser where
    empty = P (\inp -> [])
    p <|> q = P (\inp -> case parse p inp of
                            []         -> parse q inp
                            [(v, out)] -> [(v, out)]
                            _          -> [])

parse :: Parser a  -> String -> [(a, String)]
parse (P p) inp = p inp

-- Parse a single char
item :: Parser Char
item = P (\inp -> case inp of
                    (x:xs) -> [(x, xs)]
                    _      -> [])

-- Add a condition
sat :: (Char -> Bool) -> Parser Char
sat p = do x <- item
           if p x then return x else CA.empty

dissat :: (Char -> Bool) -> Parser Char
dissat p = do x <- item
              if (p x && x /= ']')  then return x else CA.empty

integ :: Parser Char
integ = do x <- item
           if (x == '-' || x == '.' || isDigit x)  then return x else CA.empty

-- Basics
digit :: Parser Char 
digit = sat isDigit

lower :: Parser Char
lower = sat isLower

upper :: Parser Char
upper = sat isUpper

letter :: Parser Char
letter = sat isAlpha

alphanum :: Parser Char
alphanum = sat isAlphaNum

char :: Char -> Parser Char
char x = sat (== x)

-- Handling spacing
space :: Parser ()
space  = do _ <- many (sat isSpace)
            return ()

-- Parse strings
string :: String -> Parser String
string [] = return []
string (x:xs) = do _ <- char x
                   _ <- string xs
                   return (x:xs)
            
-- Tokenise input
token :: Parser a  -> Parser a 
token p  = do space
              v <- p 
              space
              return v

symbol :: String -> Parser String
symbol xs = token (string xs)

ident :: Parser String
ident = do x <- many (dissat (/= ' '))
           --guard (x /= " ")
           return (x)

identifier :: Parser String
identifier = token ident



sps :: Parser Sexp
sps = do
    construct
    <|>
    atoms
    <|> 
    integers
    <|>
    doubles
    <|>
    strings


atoms :: Parser Sexp
atoms = do 
  xs <- token (string "Atom")
  col <- many (symbol ":")
  name <- token identifier
  return (Atom (T.pack (":" ++  name)))

integers :: Parser Sexp
integers = do
    xd <- token (string "Integer")
    number <- many integ
    return (Integer (read number :: Integer))

doubles :: Parser Sexp
doubles = do
    xd <- token (string "Double")
    number <- many integ
    return (Double (read number :: Double))

strings :: Parser Sexp
strings = do
    xd <- token (string "String")
    str <- token identifier
    return (String str)

construct :: Parser Sexp
construct = do
    xs <- token (char '[')
    xss <- many sps
    cls <- token (char ']')
    return (Cons xss) 