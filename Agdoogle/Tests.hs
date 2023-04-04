{-# LANGUAGE OverloadedStrings #-}
module Main where

import Test.HUnit
import System.Exit
import qualified Data.Text as DT


import Agdoogle as A
import Parsing as P
import qualified Parsing as P
import Agdoogle (textToSexp, dropNextLine)
import Data.Bool (Bool(True))


testToSexp1 = TestCase(assertEqual "testToSexp1" 
                                   (P.Atom ":anatom") 
                                   (textToSexp ("Atom :anatom")))
testToSexp2 = TestCase(assertEqual "testToSexp2" 
                                   (P.Cons [P.Atom ":first", P.Atom ":second"]) 
                                   (textToSexp ("[Atom :first Atom :second]")))
testToSexp3 = TestCase(assertEqual "testToSexp3" 
                                   (P.Cons [P.Integer 4,  String "A", P.Cons [P.Atom ":end"]]) 
                                   (textToSexp "[Integer 4 String A [Atom :end]]"))
testToSexp4 = TestCase(assertEqual "testToSexp4" 
                                   (P.Cons [P.Cons [String "nothing"],  String "A", P.Cons [P.Atom ":end"]]) 
                                   (textToSexp "[[String nothing] String A [Atom :end]]"))
testToSexp5 = TestCase(assertEqual "testToSexp5" 
                                   (P.Cons [P.Cons [P.Cons [String "useless"] ],  String "A", P.Cons [P.Atom ":end"]]) 
                                   (textToSexp "[[[String useless]] String A [Atom :end]]"))


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


dropNextLine1 = TestCase(assertEqual "dropNextLine1"
                                     "first line "
                                     (dropNextLine "first line \njust this"))
dropNextLine2 = TestCase(assertEqual "dropNextLine2"
                                     ""
                                     (dropNextLine "\njust this"))
dropNextLine3 = TestCase(assertEqual "dropNextLine3"
                                     ""
                                     (dropNextLine "\n"))


containsPath1 = TestCase(assertEqual "containsPath1"
                                     True
                                     (containsPath "AgdaDatabase/Bool" ["AgdaDatabase\\Bool"]))
containsPath2 = TestCase(assertEqual "containsPath2"
                                     False
                                     (containsPath "AgdaDatabase/Bool" ["AgdaDatabase\\extra\\Bool"]))
containsPath3 = TestCase(assertEqual "containsPath3"
                                     False
                                     (containsPath "AgdaDatabase/Bool" ["AgdaDatabaseBool"]))
containsPath4 = TestCase(assertEqual "containsPath4"
                                     True
                                     (containsPath "AgdaDatabase/extra/Bool" ["AgdaDatabase\\Bool", "AgdaDatabase/extra/Bool"]))
containsPath5 = TestCase(assertEqual "containsPath5"
                                     True
                                     (containsPath "AgdaDatabase/extra/Bool" ["AgdaDatabase\\Bool", "AgdaDatabase\\extra/Bool"]))
containsPath6 = TestCase(assertEqual "containsPath6"
                                     True
                                     (containsPath "AgdaDatabase/extra/Bool" ["AgdaDatabase\\Bool", "AgdaDatabase\\extra\\Bool"]))
containsPath7 = TestCase(assertEqual "containsPath7"
                                     True
                                     (containsPath "AgdaDatabase\\extra\\Bool" ["AgdaDatabase\\Bool", "AgdaDatabase/extra/Bool"]))

normalisePath1 = TestCase(assertEqual "normalisePath1"
                                      "AgdaDatabase/something/ornothing"
                                      (normalisePath "AgdaDatabase/something/ornothing"))
normalisePath2 = TestCase(assertEqual "normalisePath2"
                                      "AgdaDatabase/something/ornothing"
                                      (normalisePath "AgdaDatabase\\something/ornothing"))
normalisePath3 = TestCase(assertEqual "normalisePath3"
                                      "AgdaDatabase/something/ornothing"
                                      (normalisePath "AgdaDatabase/something\\ornothing"))
normalisePath4 = TestCase(assertEqual "normalisePath4"
                                      "AgdaDatabase/something/ornothing"
                                      (normalisePath "AgdaDatabase\\something\\ornothing"))

cleanName1 = TestCase(assertEqual "cleanName1"
                                  "Agda/Builtin/Primitive/something"
                                  (cleanName "Agda.Builtin.Primitive/something"))
cleanName2 = TestCase(assertEqual "cleanName2"
                                  "Agda/Builtin/Primitive/something"
                                  (cleanName "Agda/Builtin/Primitive/something"))
cleanName3 = TestCase(assertEqual "cleanName3"
                                  "something"
                                  (cleanName "something"))

testParsing1 = TestCase(assertEqual "testParsing1"
                                    [(Atom ":a", "")]
                                    (parse atoms "Atom a" ))
testParsing2 = TestCase(assertEqual "testParsing2"
                                    [(Atom ":something", "")]
                                    (parse atoms "Atom :something" ))
testParsing3 = TestCase(assertEqual "testParsing3"
                                    [(Integer 4, "")]
                                    (parse integers "Integer 4" ))
testParsing4 = TestCase(assertEqual "testParsing4"
                                    [(Integer 4927, "")]
                                    (parse integers "Integer 4927" ))
testParsing5 = TestCase(assertEqual "testParsing5"
                                    [(String "whatever", "andExtra")]
                                    (parse strings "String whatever andExtra" ))
testParsing6 = TestCase(assertEqual "testParsing6"
                                    [(Double 353232142565, "")]
                                    (parse doubles "Double 353232142565" )) 
testParsing7 = TestCase(assertEqual "testParsing7"
                                    [(Double 353242565, " 4325")]
                                    (parse doubles "Double 353242565 4325" ))
testParsing8 = TestCase(assertEqual "testParsing8"
                                    [(Cons [String "nothing"], "34")]
                                    (parse construct "[String nothing] 34" ))
testParsing9 = TestCase(assertEqual "testParsing9"
                                    [(Cons [Cons [String "nothing"], Cons [String "nothing"]], "")]
                                    (parse construct "[[String nothing] [String nothing]]" ))
testParsing10 = TestCase(assertEqual "testParsing10"
                                    [(Cons [Atom ":nothing", Cons [Atom ":modulename", Atom ":range"]], "")]
                                    (parse sps "[Atom nothing [Atom modulename Atom range]]" ))
testParsing11 = TestCase(assertEqual "testParsing11"
                                    [(Atom ":nothing", "")]
                                    (parse sps "Atom nothing" ))                                             
                                                                       



main :: IO ()
main = do
    counts <- runTestTT ( test [
        testToSexp1,
        testToSexp2,
        testToSexp3,
        testToSexp4,
        testToSexp5,
        getLineNumber1,
        getLineNumber2,
        getLineNumber3,
        getLineNumber4,
        createImports1,
        dropNextLine2,
        dropNextLine3,
        dropNextLine1,
        containsPath1,
        containsPath2,
        containsPath3,
        containsPath4,
        containsPath5,
        containsPath6,
        containsPath7,
        normalisePath1,
        normalisePath2,
        normalisePath3,
        normalisePath4,
        cleanName1,
        cleanName2,
        cleanName3,
        testParsing1,
        testParsing2,
        testParsing3,
        testParsing4,
        testParsing5,
        testParsing6,
        testParsing7,
        testParsing8,
        testParsing9,
        testParsing10,
        testParsing11
        ])
    if (errors counts + failures counts == 0)
        then exitSuccess
        else exitFailure