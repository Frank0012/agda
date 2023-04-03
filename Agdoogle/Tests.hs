{-# LANGUAGE OverloadedStrings #-}
module Main where

import Test.HUnit
import System.Exit
import qualified Data.Text as DT


import Agdoogle as A
import Parsing as P
import qualified Parsing as P
import Agdoogle (textToSexp, dropNextLine)


testToSexp1 = TestCase(assertEqual "testToSexp1" 
                                   (P.Atom ":anatom") 
                                   (textToSexp ("Atom :anatom")))
testToSexp2 = TestCase(assertEqual "testToSexp2" 
                                   (P.Cons [P.Atom ":first", P.Atom ":second"]) 
                                   (textToSexp ("[Atom :first Atom :second]")))
testToSexp3 = TestCase(assertEqual "testToSexp3" 
                                   (P.Cons [P.Integer 4,  String "A", P.Cons [P.Atom ":end"]]) 
                                   (textToSexp "[Integer 4 String A [Atom :end]]"))

getLineNumber1 = TestCase(assertEqual "getLineNumber1" 
                                      (2) 
                                      (getLineNumber ["first line number", "second line number", "third line number"] "second line number"))
getLineNumber2 = TestCase(assertEqual "getLineNumber2"
                                      (0)
                                      (getLineNumber [] ""))
getLineNumber3 = TestCase(assertEqual "getLineNumber3"
                                      (2)
                                      (getLineNumber ["unimpotant", "useless"] "useless"))
getLineNumber4 = TestCase(assertEqual "getLineNumber4"
                                      (0)
                                      (getLineNumber [] "this will never be found in the list"))


createImports1 = TestCase(assertEqual "createImports1"
                                      ["open import Useless", "open import MoreUseless", "open import Agda.Primitive"]
                                      (createImports ["Useless.agda", "MoreUseless.agda"]))


main :: IO ()
main = do
    counts <- runTestTT ( test [
        testToSexp1,
        testToSexp2,
        testToSexp3,
        getLineNumber1,
        getLineNumber2,
        getLineNumber3,
        getLineNumber4,
        createImports1
        ])
    if (errors counts + failures counts == 0)
        then exitSuccess
        else exitFailure