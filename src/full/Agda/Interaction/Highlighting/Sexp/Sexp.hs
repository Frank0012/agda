-- | A very simple implementation of S-expressions that can be dumped to Text easily

{-# LANGUAGE OverloadedStrings #-}

module Agda.Interaction.Highlighting.Sexp.Sexp where
import Debug.Trace
import Data.Word
import Data.Text.Lazy (Text)
import qualified Data.Text.Lazy as T
import qualified Data.Text as DT
import Data.Text.Lazy.IO as W

import Data.Char (isDigit)

import Data.Text (splitOn, pack)
import Control.Monad (guard)
import Control.Monad.Trans (MonadIO(..))

data Sexp = Atom Text | String String | Integer Integer | Double Double | Cons [Sexp]
            deriving (Show, Eq)

constr :: String -> [Sexp] -> Sexp
constr head lst = Cons (Atom (':' `T.cons` (T.pack head)) : lst) --trace ("CALLING    " ++ show (Cons (Atom (':' `T.cons` (T.pack head)) : lst)))  (Cons (Atom (':' `T.cons` (T.pack head)) : lst))

toText :: Sexp -> T.Text
toText (Atom x)   = x
toText (Integer k) = T.pack $ show k
toText (Double x) = T.pack $ show x
toText (String s) = T.pack $ show s
toText (Cons lst) = '(' `T.cons` (T.intercalate (T.singleton ' ') (map toText lst)) `T.snoc` ')'

-- | Given a function name and a sexp, return the top level definition clause matching that function name, wrapped in a list
findList :: T.Text -> Sexp -> [Sexp]
findList name (Cons mod) = do
  (Cons ((Atom ":definition") : ((Cons spls) : spss)))  <- mod
  (Atom something) <- spls
  guard (something == name)
  return (Cons ((Atom ":definition") : ((Cons spls) : spss)))
findList name _ = [String "nothing"]

-- | Output found function from agda module to file
--search :: String -> Sexp -> IO ()
--search name mod = W.writeFile "test/result.txt" toWrite
--    where
--        result = findList (T.pack name) mod
--        toWrite = toText (constr "result" result)

search :: MonadIO m => String -> Sexp -> m ()
search name mod = liftIO (W.writeFile "resulting.txt" toWrite)
    where
        result = findList (T.pack name) mod
        toWrite = toText (constr "result" result)
    
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
