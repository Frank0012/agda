module Agda.Interaction.Highlighting.Sexp.Search where

import Debug.Trace
import Data.Word
import Data.Text.Lazy (Text)
import qualified Data.Text.Lazy as T
import qualified Data.Text as DT

import Agda.Interaction.Highlighting.Sexp.Sexp

import Control.Monad (guard)

-- | Given a function name and a sexp, return the top level definition clause matching that function name

---find :: String -> Sexp -> Sexp
---find name (Cons xs) = Cons (findList name (xs))

--findList :: String -> [Sexp] -> [Sexp]
--findList name (name:[]) = name:[]
---findList name ((Atom ":definition") : ((Cons xs) : xss)) = findList name ((Atom ":definition") : ((Cons (findList name xs)) : xss))
--findList name (Atom)
--findList name (x:xs) = x ++ (findList name xs)


findList :: String -> [Sexp] -> Sexp
findList name sps = do
  a  <- as
  guard (a == Cons ((Atom ":definition") : spss))
  b <- spss
  guard (b == name)
  return (a)