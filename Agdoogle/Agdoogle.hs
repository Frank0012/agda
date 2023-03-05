-- | A very simple implementation of S-expressions that can be dumped to Text easily

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}

module Agdoogle where
--import Agda.Interaction.Highlighting.Sexp.Sexp
import Debug.Trace
import Data.Word
--import Data.Text.Lazy (Text)
import qualified Data.Text.Lazy as T
import qualified Data.Text as DT
import Data.Text.Lazy.IO as W

import qualified Data.Text.IO as TIO


import System.Process
import System.Info

import Control.Applicative ( Alternative(many, (<|>), empty) )
import qualified Control.Applicative as CA
import Data.Char
--import qualified Data.Text.IO as DTI
--import Data.Text.Internal as DTin

import Data.Char (isDigit, digitToInt)

--import Data.Text (splitOn, pack)
import Control.Monad (guard)
--import Control.Monad.Trans (MonadIO(..))


import System.IO
import Data.Text (Text, index, count)
import GHC.Float (fromRat'')


------------TO INVESTIGATE: MULTIPLE RESULTS--------------------

textToSexp :: DT.Text -> Sexp
textToSexp text = fst . head  $ parse sps (DT.unpack text) --(trace (DT.unpack text) (DT.unpack text))

agdoogle :: IO [Sexp]
agdoogle = do
    W.putStrLn "Enter file to search"
    databaseFile <- Prelude.getLine
    W.putStrLn "Name search or type search?" 
    W.putStrLn "[N] = name"
    W.putStrLn "[T] = type"
    selection <- Prelude.getLine
    database <- TIO.readFile ("SexpDatabase/" ++ databaseFile)
    if selection == "T" 
    then do W.putStrLn "Enter type to search"
            type' <- Prelude.getLine
            replaceType type'
            compile
            searchTerm <- TIO.readFile "SexpDatabase/searchTerm.agda-sexp"
            -- Returning range inromation here
            trace (show ([ returnRangeData x | x <- findType (extractTypeFromSearch (textToSexp searchTerm)) $ (textToSexp database)])) (return "[]")
            -- End of returning ranged information
            return (findType (extractTypeFromSearch (textToSexp searchTerm)) $ (textToSexp database))
    else do W.putStrLn "Enter name"
            name  <- W.getLine
            return (findName name $ (textToSexp database))

replaceType :: String -> IO ()
replaceType type' = do
  contents <- TIO.readFile "SearchTerm/searchTerm.agda"
  let modifiedContents = DT.unlines $ map replaceLine (DT.lines contents)
  withFile "SearchTerm/searchTerm.agda" WriteMode $ \handle -> do
    TIO.hPutStr handle modifiedContents
  where
    replaceLine line
      | DT.isInfixOf (DT.pack ":") line = DT.pack ("searchTerm : " ++ type')
      | otherwise = line

---- TODO: FIX THIS ----
compile :: IO ()
compile = do
  let cmd = case os of
              "linux" -> "x-terminal-emulator -e bash " ++ "compile.sh"
              "darwin" -> "open -a Terminal " ++ "compile.sh"
              "mingw32" -> "start " ++ "%HOMEPATH%/Documents/University/finalYearProject/finalYearGithub/agda/Agdoogle/compile.sh"
              _ -> error "Unsupported operating system"
  callCommand cmd

melt :: [Sexp] -> Sexp
melt []   = String "empty"
melt x = Cons x

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
----- WHAT ABOUT MULTIPLE RESULTS???? -----
findType :: [Sexp] -> Sexp -> [Sexp]
findType type' (Cons mod) = --do
    --trace ("trying to find this type " ++ (show type')) ("wassup")
    --(Cons ((Atom ":definition") : ((Cons name) : ((Cons types) : defs))))  <- mod
    --trace ("supposedly the matching type " ++ (show types)) ("hi")
    --guard (removeRangeFromType types == removeRangeFromType type')
    --return (constr "RESULT" ((Atom ":definition") : ((Cons name) : ((Cons types) : defs))))

    [Cons ((Atom ":definition") : ((Cons name) : ((Cons types) : defs))) | 
    (Cons ((Atom ":definition") : ((Cons name) : ((Cons types) : defs)))) <- mod, 
    removeRangeFromType types == removeRangeFromType type']
findType type' _ = [String "nothing found"]

removeRangeFromType :: [Sexp] -> [Sexp]
removeRangeFromType [] = []
removeRangeFromType (Cons (Atom ":position" : (Integer n : (Integer x : (Integer q : [])))) : more) = (Atom ":position") : (removeRangeFromType more)
removeRangeFromType ((Cons s) : more) = (Cons (removeRangeFromType s)) : removeRangeFromType more
removeRangeFromType (s:str) = s : removeRangeFromType str

-- | Return position information from the found definition
-- definition is made up of : cons definition [[name], [type], [defn]]
-- where defn is a definition from TypeCheckingMond.Defn. [defn] has the 
-- location information that we need
-- defn for datatype = [Atom :data [[sort] [name of first constructor] [name of second constructor] [name of third...etc]]]
-- constructors are of the form = [Atom :name [[module name], [datatype name], [constructor name]]]
-- constructor names are of the form = [Atom :finalname, Atom thename, [Atom :range, [range], [Atom :intervalwithoutfile, [Atom :interval, [startposition], [endposition] ]]]]
-- | TODO : MIGHT NEED TO UPDATE THIS IMPLEMENTATION TO DEAL WITH PROPER SEXP CONSTRUCTION FROM THE BACKEND. IE: CONS [CONS[CONS[]]] (RIGHT DEEP TREE)
returnRangeData :: Sexp -> [Sexp]
returnRangeData (Cons [definitionAtom , Cons names, types, (Cons datas)]) = do
    
    -- Get data type name location
    Cons dataName <- tail (tail names)
    Cons [Atom rng, range, Cons [Atom intervalwithoutfile, Cons [Atom interval, Cons startpos, Cons endpos], extra]] <- dataName

    -- Now get the constructor names location
    constructorList <- tail (tail datas)
    let ranges = [head (reverse x) | (Cons x) <- tail (tail datas)]
    
    let constructorRanges = [ Cons (constructorstartpos ++ constructorendpos) | Cons [ Atom finnme, Atom nme,  Cons [Atom constructorrng, constructorrange, Cons [Atom constructorintervalwithoutfile, Cons [Atom constructorinterval, Cons constructorstartpos, Cons constructorendpos], constructorextra]]] <- ranges ]
    
    return (Cons [Cons (startpos ++ endpos) , Cons constructorRanges])
returnRangeData f = trace (show f) [String "nothing"]

-- | Return the line number which the given character postition falls on
getLineFromString :: Int -> String -> String
getLineFromString num str = reverse (dropNextLine (reverse (take num str))) ++ (dropNextLine (drop num str))

dropNextLine :: String -> String
dropNextLine [] = []
dropNextLine ('\n' : str) = ""
dropNextLine (s:str) = s : dropNextLine str




    

data Sexp = Atom T.Text | String String | Integer Integer | Double Double | Cons [Sexp]
            deriving (Show, Eq)

constr :: String -> [Sexp] -> Sexp
constr head lst = Cons (Atom (':' `T.cons` (T.pack head)) : lst) --trace ("CALLING    " ++ show (Cons (Atom (':' `T.cons` (T.pack head)) : lst)))  (Cons (Atom (':' `T.cons` (T.pack head)) : lst))

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