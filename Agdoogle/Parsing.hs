module Parsing where

import qualified Data.Text.Lazy as T
import Control.Applicative ( Alternative(many, (<|>), empty) )
import qualified Control.Applicative as CA
import Data.Char

data Sexp = Atom T.Text | String String | Integer Integer | Double Double | Cons [Sexp]
            deriving (Show, Eq)

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
token p  = do _ <- space
              v <- p 
              _ <- space
              return v

symbol :: String -> Parser String
symbol xs = token (string xs)

ident :: Parser String
ident = do x <- many (dissat (/= ' '))
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
  _ <- token (string "Atom")
  _ <- many (symbol ":")
  name <- token identifier
  return (Atom (T.pack (":" ++  name)))

integers :: Parser Sexp
integers = do
    _ <- token (string "Integer")
    number <- many integ
    return (Integer (read number :: Integer))

doubles :: Parser Sexp
doubles = do
    _ <- token (string "Double")
    number <- many integ
    return (Double (read number :: Double))

strings :: Parser Sexp
strings = do
    _ <- token (string "String")
    str <- token identifier
    return (String str)

construct :: Parser Sexp
construct = do
    _ <- token (char '[')
    xss <- many sps
    _ <- token (char ']')
    return (Cons xss) 