-- Entry point for Agdoogle search command line utility
module Main where

import Agdoogle
import Main.Utf8 (withUtf8)

main :: IO ()
main = withUtf8 $ do
    agdoogle
    