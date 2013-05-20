module Meriwether.GDL.Parser (
  parseGDL
  ) where

-- System
import Control.Monad

-- External
import Text.ParserCombinators.Parsec

-- Internal
import Meriwether.GDL.Model


-- Interface


parseGDL :: String -> Either ParseError [Expression]
parseGDL input = parse documentP "" input


-- Parser chain

documentP :: Parser [Expression]
documentP = do
  -- Skip any comments or spaces at the beginning of the document
  skipMany space >> commentP
  -- Parse expressions separated by spaces and optionally comments
  sepEndBy expressionP (spaces >> commentP)

commentP :: Parser ()
commentP = optional $ do 
  string ";"
  manyTill anyChar newline
  return ()

expressionP :: Parser Expression
expressionP = (InferenceRule `liftM` try ruleP) <|>
              (Ground `liftM` atomP)

ruleP :: Parser Rule
ruleP = do
  char '(' >> spaces >> string "<=" >> spaces
  conclusion <- atomP
  spaces
  predicates <- literalP `sepBy1` spaces
  spaces >> char ')'
  return $ conclusion :- predicates

atomP :: Parser Atom
atomP = propP <|> relationP

literalP :: Parser Literal
literalP = (try negationP) <|>
           (try distinctP) <|>
           (Pos `liftM` atomP)

propP :: Parser Atom
propP = Proposition `liftM` nameP

relationP :: Parser Atom
relationP = do
  char '(' >> spaces
  name <- nameP
  spaces
  terms <- termP `sepBy` spaces
  spaces >> char ')'
  return $ Relation name terms

negationP :: Parser Literal
negationP = do
  char '(' >> spaces >> string "not" >> spaces
  arg <- atomP
  spaces >> char ')'
  return $ Neg arg

distinctP :: Parser Literal
distinctP = do
  char '(' >> spaces >> string "distinct" >> spaces
  a <- termP
  spaces
  b <- termP
  spaces >> char ')'
  return $ Distinct a b

termP :: Parser Term
termP = varP <|> constP <|> functionP

varP :: Parser Term
varP = char '?' >> (Var `liftM` nameP)

constP :: Parser Term
constP = Const `liftM`  nameP

functionP :: Parser Term
functionP = do
  char '(' >> spaces
  name <- nameP
  spaces
  terms <- termP `sepBy` spaces
  spaces >> char ')'
  return $ Function name terms

nameP :: Parser Name
nameP = many1 $ letter <|> digit <|> char '_'
