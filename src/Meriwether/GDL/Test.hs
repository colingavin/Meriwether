module Meriwether.GDL.Test (
    unitTests
    ) where

import Test.HUnit
import Text.ParserCombinators.Parsec

import Meriwether.GDL.Parser
import Meriwether.GDL.Model


-- Unit tests for the GDL parser
unitTests = TestList [TestLabel "Should parse atoms" atomTests,
                       TestLabel "Should parse logical compounds" compoundTests,
                       TestLabel "Should parse general expressions" expressionTests]

genericParserTest :: String -> Either ParseError GDLDocument -> Test

genericParserTest str (Right expected) = case parseGDL str of
    Right result -> TestCase $ assertEqual str expected result
    Left err -> TestCase $ assertFailure $ "Got parse error " ++ (show err) ++ " but expected " ++ (show expected)

genericParserTest str (Left expected) = case parseGDL str of
    Right result -> TestCase $ assertFailure $ "Expected parse error but got " ++ (show result)
    Left err -> TestCase $ assertEqual str (show expected) (show err)

-- Tests regarding parsing atomic expressions
atomTests = TestList [TestLabel "Should parse constants" symbolTest,
                      TestLabel "Should parse variables" variableTest]

symbolTest = genericParserTest "true1" $ Right [GDLConst "true1"]

variableTest = genericParserTest "?x" $ Right [GDLVar "x"]

-- Tests regarding logical compounds
compoundTests = TestList [TestLabel "Should parse negation" negationTest,
                           TestLabel "Should parse conjunction" conjunctionTest,
                           TestLabel "Should parse implication" implicationTest,
                           TestLabel "Should parse implicit conjunction" implicitConjunctionTest]

negationTest = genericParserTest "(not ?x)" $ Right [GDLNot (GDLVar "x")]

conjunctionTest = genericParserTest "(and ?x ?y)" $ Right [GDLAnd (GDLVar "x") (GDLVar "y")]

implicationTest = genericParserTest "(<= ?y ?x)" $ Right [GDLImplies (GDLVar "x") (GDLVar "y")]

implicitConjunctionTest = genericParserTest "(<= ?z ?x ?y)" $ Right [GDLImplies (GDLAnd (GDLVar "x") (GDLVar "y")) (GDLVar "z")]

-- Tests regarding general expressions
expressionTests = TestList [TestLabel "Should parse head only expressions" headOnlyTest,
                             TestLabel "Should parse 1-arity expressions" arityOneTest,
                             TestLabel "Should parse 2-arity expressions" arityTwoTest,
                             TestLabel "Should parse nested expression" nestedTest,
                             TestLabel "Should parse multisentence document" documentTest]

headOnlyTest = genericParserTest "(true)" $ Right [GDLSentence (GDLConst "true") []]

arityOneTest = genericParserTest "(role x)" $ Right [GDLSentence (GDLConst "role") [GDLConst "x"]]

arityTwoTest = genericParserTest "(row a b)" $ Right [GDLSentence (GDLConst "row") [GDLConst "a", GDLConst "b"]]

nestedTest = genericParserTest "(next (at 2))" $ Right [GDLSentence (GDLConst "next") [GDLSentence (GDLConst "at") [GDLConst "2"]]]

documentTest = genericParserTest "\n(row 1 2)\n ;comment \n(row 1 1)" $ Right expect
    where expect = [GDLSentence (GDLConst "row") [GDLConst "1", GDLConst "2"], 
                    GDLSentence (GDLConst "row") [GDLConst "1", GDLConst "1"]]
