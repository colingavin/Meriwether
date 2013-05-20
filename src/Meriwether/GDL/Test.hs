module Meriwether.GDL.Test (
    unitTests
    ) where

import Test.HUnit
import Text.ParserCombinators.Parsec

import Meriwether.GDL.Parser
import Meriwether.GDL.Model
import Meriwether.GDL.Substitution
import Meriwether.GDL.Unification
import Meriwether.GDL.Query


-- Unit tests for the GDL handling code
unitTests = TestList [TestLabel "Should parse atoms" atomTests,
                      TestLabel "Should parse logical compounds" compoundTests,
                      TestLabel "Should parse general expressions" expressionTests,
                      TestLabel "Should substitute correctly" substitutionTests,
                      TestLabel "Should unify expressions correctly" unificationTests,
                      TestLabel "Should query expressions correctly" queryTests]

genericParserTest :: String -> Either ParseError [Expression] -> Test

genericParserTest str (Right expected) = case parseGDL str of
    Right result -> TestCase $ assertEqual str expected result
    Left err -> TestCase $ assertFailure $ "Got parse error " ++ (show err) ++ " but expected " ++ (show expected)

genericParserTest str (Left expected) = case parseGDL str of
    Right result -> TestCase $ assertFailure $ "Expected parse error but got " ++ (show result)
    Left err -> TestCase $ assertEqual str (show expected) (show err)


-- Tests regarding parsing atomic expressions
atomTests = TestList [TestLabel "Should parse propostions" propositionTest]

propositionTest = genericParserTest "final" $ Right [Ground $ Proposition "final"]


-- Tests regarding logical compounds
compoundTests = TestList [TestLabel "Should parse negation" negationTest,
                          TestLabel "Should parse conjunction" conjunctionTest,
                          TestLabel "Should parse distinct" distinctTest]

negationTest = genericParserTest "(<= final (not (f ?x)))" $ 
  Right [InferenceRule $ Proposition "final" :- [Neg $ Relation "f" [Var "x"]]]

conjunctionTest = genericParserTest "(<= final (f ?x) (g ?x))" $ 
  Right [InferenceRule $ Proposition "final" :- [Pos $ Relation "f" [Var "x"], Pos $ Relation "g" [Var "x"]]]

distinctTest = genericParserTest "(<= final (distinct ?x ?y))" $ 
  Right [InferenceRule $ Proposition "final" :- [Distinct (Var "x") (Var "y")]]


-- Tests regarding general expressions
expressionTests = TestList [TestLabel "Should parse head only expressions" headOnlyTest,
                            TestLabel "Should parse 1-arity expressions" arityOneTest,
                            TestLabel "Should parse 2-arity expressions" arityTwoTest,
                            TestLabel "Should parse nested expression" nestedTest,
                            TestLabel "Should parse multisentence document" documentTest]

headOnlyTest = genericParserTest "(true)" $ Right [Ground $ Relation "true" []]

arityOneTest = genericParserTest "(role x)" $ Right [Ground $ Relation "role" [Const "x"]]

arityTwoTest = genericParserTest "(row a b)" $ Right [Ground $ Relation "row" [Const "a", Const "b"]]

nestedTest = genericParserTest "(next (at 2))" $ Right [Ground $ Relation "next" [Function "at" [Const "2"]]]

documentTest = genericParserTest "\n(row 1 2)\n ;comment \n(row 1 1)" $ Right expect
    where expect = [Ground $ Relation "row" [Const "1", Const "2"],
                    Ground $ Relation "row" [Const "1", Const "1"]]


-- Tests regarding substitution
substitutionTests = TestList [TestLabel "Vars should substitute" varSubTest,
                              TestLabel "Function args should substitute" funcSubTest,
                              TestLabel "Relation args should substitute" relationSubTest,
                              TestLabel "Negation should substitute" negationSubTest,
                              TestLabel "Distinct should substitute" distinctSubTest,
                              TestLabel "Rule conclusion should substitute" ruleSubTest]

genericSubstitutionTest :: Substitute a => Substitution -> a -> a -> Test
genericSubstitutionTest sub start expect = TestCase $ assertEqual (show sub) (substitute sub start) expect

testSubstitution = Var "x" :~> Const "a"

varSubTest = genericSubstitutionTest testSubstitution (Var "x") (Const "a")

funcSubTest = genericSubstitutionTest testSubstitution (Function "f" [Var "x"]) (Function "f" [Const "a"])

relationSubTest = genericSubstitutionTest testSubstitution (Relation "f" [Var "x"]) (Relation "f" [Const "a"])

negationSubTest = genericSubstitutionTest testSubstitution (Neg $ Relation "f" [Var "x"]) (Neg $ Relation "f" [Const "a"])

distinctSubTest = genericSubstitutionTest testSubstitution 
  (Distinct (Function "f" [Var "x"]) (Function "g" [Var "y"]))
  (Distinct (Function "f" [Const "a"]) (Function "g" [Var "y"]))

ruleSubTest = genericSubstitutionTest testSubstitution
  (Relation "f" [Var "x"] :- [Pos $ Relation "g" [Var "x"]])
  (Relation "f" [Const "a"] :- [Pos $ Relation "g" [Const "a"]])


-- Tests regarding unification

unificationTests = TestList [TestLabel "Should unify variables with constants" varUnifyTest,
                             TestLabel "Should unify functions" funcUnifyTest,
                             TestLabel "Should unify relations" relationUnifyTest,
                             TestLabel "Should unify rules with relations" ruleUnifyTest,
                             TestLabel "Should unify rules in both directions" emptyRuleUnifyTest,
                             TestLabel "Should unify relations at multiple levels" relationLevelUnifyTest,
                             TestLabel "Should unify ground rules with relations" groundRuleUnifyTest,
                             TestLabel "Should unify identical relations" sameRelationUnifyTest,
                             TestLabel "Should unify identical terms" sameTermUnifyTest,
                             TestLabel "Should not unify conflicting functions (name)" badFuncNameUnifyTest,
                             TestLabel "Should not unify conflicting relations (name)" badRelationNameUnifyTest,
                             TestLabel "Should not unify conflicting relations (args)" badRelationArgsUnifyTest]

genericUnificationTest :: (Unify a, Unify b) => a -> b -> Maybe [Substitution] -> Test
genericUnificationTest a b expect = TestCase $ assertEqual desc expect (unify a b)
  where desc = (show a) ++ " & " ++ (show b) ++ " => " ++ (show expect)

varUnifyTest = genericUnificationTest (Var "x") (Const "a") $ Just [Var "x" :~> Const "a"]

funcUnifyTest = genericUnificationTest (Function "f" [Var "x"]) (Function "f" [Const "a"]) $ Just [Var "x" :~> Const "a"]

relationUnifyTest = genericUnificationTest (Relation "f" [Var "x"]) (Relation "f" [Const "a"]) $ Just [Var "x" :~> Const "a"]

ruleUnifyTest = genericUnificationTest 
  (Relation "f" [Var "x"] :- [Pos $ Relation "g" [Var "x"]]) 
  (Relation "f" [Const "a"]) 
  $ Just [Var "x" :~> Const "a"]

emptyRuleUnifyTest = genericUnificationTest
  (Relation "f" [Const "a"] :- [])
  (Relation "f" [Var "x"])
  $ Just [Var "x" :~> Const "a"]

relationLevelUnifyTest = genericUnificationTest 
  (Relation "f" [Function "g" [Var "x"]])
  (Relation "f" [Function "g" [Const "a"]])
  $ Just [Var "x" :~> Const "a"]

groundRuleUnifyTest = genericUnificationTest
  (Relation "f" [Const "a"] :- [])
  (Relation "f" [Const "a"])
  $ Just []

sameRelationUnifyTest = genericUnificationTest rel rel $ Just []
  where rel = Relation "f" [Const "a"]

sameTermUnifyTest = genericUnificationTest (Const "a") (Const "a") $ Just []

badFuncNameUnifyTest = genericUnificationTest (Function "f" [Var "x"]) (Function "g" [Const "a"]) Nothing

badRelationNameUnifyTest = genericUnificationTest (Relation "f" [Var "x"]) (Relation "g" [Const "a"]) Nothing

badRelationArgsUnifyTest = genericUnificationTest 
  (Relation "f" [Function "g" [Var "x"], Function "h" [Var "x"]])
  (Relation "f" [Function "g" [Const "a"], Function "h" [Const "b"]])
  Nothing


-- Tests regarding queries

queryTests = TestList [TestLabel "Should normalize relations correctly" relNormalizationTest,
                       TestLabel "Should normalize rules correctly" ruleNormalizationTest,
                       TestLabel "Should query ground correctly" groundQueryTest,
                       TestLabel "Should query simple negations correctly" negatedGroundQueryTest,
                       TestLabel "Should solve one predicate implication" basicImplicationQueryTest,
                       TestLabel "Should solve implication with free variable" freeVarQueryTest]

relNormalizationTest = TestCase $ assertEqual desc expect (deductiveSystem (map Ground rels))
  where desc = (show rels) ++ " => " ++ (show expect)
        expect = DeductiveSystem $ [(head rels) :- []]
        rels = [Relation "f" [Const "a"]]

ruleNormalizationTest = TestCase $ assertEqual desc expect (deductiveSystem exprs)
  where desc = (show exprs) ++ " => " ++ (show expect)
        expect = DeductiveSystem [Relation "f" [Var "x"] :- [Pos $ Relation "g" [ExistVar "z"]]]
        exprs = [InferenceRule $ (Relation "f" [Var "x"]) :- [Pos $ Relation "g" [Var "z"]]]


genericQueryTest :: (Queryable a) => (Query [Substitution] -> [[Substitution]]) -> a -> DeductiveSystem -> [[Substitution]] -> Test
genericQueryTest f q ds expect = TestCase $ assertEqual desc expect (f $ query q ds)
  where desc = (show q) ++ " => " ++ (show expect) ++ " in " ++ (show ds)

genericQueryTestAll :: (Queryable a) => a -> DeductiveSystem -> [[Substitution]] -> Test
genericQueryTestAll = genericQueryTest observeAllQuery

genericQueryTestMany :: (Queryable a) => Int -> a -> DeductiveSystem -> [[Substitution]] -> Test
genericQueryTestMany n = genericQueryTest (observeManyQuery n)

groundQueryTest = genericQueryTestAll rel ds [[]]
  where ds = deductiveSystem [Ground rel]
        rel = Relation "f" [Const "a"]

negatedGroundQueryTest = genericQueryTestAll rel ds [[]]
  where ds = deductiveSystem [Ground $ Relation "f" [Const "a"]]
        rel = Neg $ Relation "f" [Const "b"]

basicImplicationQueryTest = genericQueryTestAll rel ds [[Var "x" :~> Const "a"]]
  where ds = deductiveSystem [Ground $ Relation "f" [Const "a"],
                              InferenceRule $ Relation "g" [Var "x"] :- [Pos $ Relation "f" [Var "x"]]]
        rel = Relation "g" [Var "x"]

freeVarQueryTest = genericQueryTestAll q ds [[]]
  where ds = deductiveSystem [Ground $ Relation "g" [Const "a"],
                              InferenceRule $ Proposition "p" :- [Pos $ Relation "g" [Var "z"]]]
        q = Proposition "p"
