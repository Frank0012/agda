-- | A very simple implementation of S-expressions that can be dumped to Text easily

{-# LANGUAGE OverloadedStrings #-}

module Agda.Interaction.Highlighting.Sexp.Sexp where
import Debug.Trace
import Data.Word
import Data.Text.Lazy (Text)
import qualified Data.Text.Lazy as T
import qualified Data.Text as DT
import Data.Text.Lazy.IO as W


import Control.Applicative
import Data.Char

import Data.Char (isDigit, digitToInt)

import Data.Text (splitOn, pack)
import Control.Monad (guard)
import Control.Monad.Trans (MonadIO(..))

--import ParseAgda
--import Agda.Interaction.Highlighting.Sexp.Parsing.ParseSexp
--import Agda.Interaction.Highlighting.Sexp.Parsing.Primitives


data Sexp = Atom Text | String String | Integer Integer | Double Double | Cons [Sexp]
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
  return (Atom (T.pack (":" ++ name)))

integers :: Parser Sexp
integers = do
    xd <- token (string "Integer")
    number <- many digit
    return (Integer (read number :: Integer))

doubles :: Parser Sexp
doubles = do
    xd <- token (string "Double")
    number <- many digit
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


textToSexp :: T.Text -> Sexp
textToSexp text = fst . head  $ parse sps (T.unpack text)


-- | Given a function name and a sexp, return the top level definition clause matching that function name, wrapped in a list
findName :: T.Text -> Sexp -> [Sexp]
findName name (Cons mod) = do
  (Cons ((Atom ":definition") : ((Cons spls) : spss)))  <- mod
  (Atom something) <- spls
  guard (something == name)
  return (Cons ((Atom ":definition") : ((Cons spls) : spss)))
findName name _ = [String "nothing found"]

-- | Given a sexp of a type signature, return the top level definition clause matching that type signature, wrapped in a list
----- WHAT ABOUT MULTIPLE RESULTS???? -----
findType :: Sexp -> Sexp -> [Sexp]
findType type' (Cons mod) = do
    (Cons ((Atom ":definition") : ((Cons name) : ((types) : functions))))  <- mod
    guard (types == type')
    return (Cons ((Atom ":definition") : ((Cons name) : ((types) : functions))))
findType type' _ = [String "nothing found"]



-- | Output found function from agda module to file
--search :: String -> Sexp -> IO ()
--search name mod = W.writeFile "test/result.txt" toWrite
--    where
--        result = findList (T.pack name) mod
--        toWrite = toText (constr "result" result)

--search :: MonadIO m => String -> Sexp -> m ()
--search name mod = liftIO (W.writeFile "resulting.txt" toWrite)
--    where
--        result = findName (T.pack name) mod
--        toWrite = toText (constr "result" result)

search :: MonadIO m => Sexp -> Sexp -> m ()
search querey mod = liftIO (W.writeFile "resulting.txt" toWrite)
    where
        result = findType querey mod
        toWrite = toText (constr "result" (trace (show result) (result)))
    
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
           if p x then return x else empty

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
ident = do x <- many alphanum
           return (x)

identifier :: Parser String
identifier = token ident