module Main where

--import Agdoogle
import Main.Utf8 (withUtf8)
import Debug.Trace
import Data.Text.Lazy.IO as T

--main :: IO ()
--main = withUtf8 $ do
--    agdoogle

main :: IO () 
main = withUtf8 $ do
    line <- T.getLine
    trace ("here is the line = " ++ show line) (return "hi")
    --text <- T.readFile "AgdaDatabase/Builtin.agda"
    T.putStr line