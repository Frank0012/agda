module Main where

--import System.Info
import Agdoogle
import Main.Utf8 (withUtf8)
--import Debug.Trace
--import qualified Data.Text.IO as TIO
--import Data.Text as DT
--import System.IO
--import qualified System.IO.Utf8 as Utf8

--writeData :: Handle -> Text -> IO ()
--writeData hOut inData = Utf8.withHandle hOut $ do
--  TIO.hPutStr hOut inData

main :: IO ()
main = withUtf8 $ do
    agdoogle

--main :: IO String
--main = if os == "mingw32" then return ("windows") else return ("linux or mac")

--main :: IO () 
--main = withUtf8 $ do
--    line <- System.IO.getLine
--    replaceType line

--replaceType :: String -> IO ()
--replaceType type' = withUtf8 $ do
--  contents <- TIO.readFile "AgdaDatabase/searchTerm.agda"
--  let modifiedContents = DT.unlines $ Prelude.map replaceLine (DT.lines contents)
--  withFile "AgdaDatabase/searchTerm.agda" WriteMode $ \handle -> do
--    writeData handle (DT.pack type')
--  where
--    replaceLine line
--      | DT.isInfixOf (DT.pack ":") line = DT.pack ("searchTerm : " ++ type')
--      | otherwise = line