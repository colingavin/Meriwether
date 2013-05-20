module Main where

import Test.HUnit
import System.Exit (exitFailure, exitSuccess)

import Meriwether.GDL.Test as GDL


allTests :: Test
allTests = TestList [TestLabel "GDL" GDL.unitTests]

main = do
    results <- runTestTT allTests
    if (failures results > 0) || (errors results > 0)
        then exitFailure
        else exitSuccess
