module Tests where

import Test.HUnit
import System.Exit


import Agdoogle as A

testZeroCase = TestCase(assertEqual "1" (1) (1))


main :: IO ()
main = do
    counts <- runTestTT ( test [
        testZeroCase,
        testOneCase
        ])
    if (errors counts + failures counts == 0)
        then exitSuccess
        else exitFailure