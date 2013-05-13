module Main where

import Test.HUnit
import System.Exit (exitFailure, exitSuccess)

import Meriwether.GDL.Test as GDL


allTests :: Test
allTests = TestList [TestLabel "GDL" GDL.unitTests]

main = do
    counts <- runTestTT allTests
    if failures counts > 0
        then exitFailure
        else exitSuccess
