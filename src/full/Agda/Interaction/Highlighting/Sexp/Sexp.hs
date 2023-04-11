-- | A very simple implementation of S-expressions that can be dumped to Text easily

{-# LANGUAGE OverloadedStrings #-}

module Agda.Interaction.Highlighting.Sexp.Sexp where

import Data.Word
import Data.Text.Lazy (Text)
import qualified Data.Text.Lazy as T
import qualified Data.Text as DT

data Sexp = Atom Text | String String | Integer Integer | Double Double | Cons [Sexp]
            deriving (Show, Eq)

constr :: String -> [Sexp] -> Sexp
constr head lst = Cons (Atom (':' `T.cons` (T.pack head)) : lst) 

-- | Frank Crossley: add labels for text output to aid parsing
toText :: Sexp -> T.Text
toText (Atom x)   = T.pack $ "Atom " ++ T.unpack x ++ " "
toText (Integer k) = T.pack $ "Integer " ++ show k ++ " "
toText (Double x) = T.pack $ "Double " ++ show x ++ " "
toText (String s) = T.pack $ "String " ++ s ++ " "
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


