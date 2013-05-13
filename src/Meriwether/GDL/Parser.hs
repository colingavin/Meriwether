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

parseGDL :: String -> Either ParseError GDLDocument
parseGDL input = parse documentP "" input


-- Parser chain

documentP :: Parser GDLDocument
documentP = do
    -- Skip any comments or spaces at the beginning of the document
    skipMany spaces >> commentP
    -- Parse expressions separated by spaces and optionally comments
    sepEndBy expressionP (spaces >> commentP)

expressionP :: Parser GDLExpression
expressionP = constP <|> varP <|> (try notP) <|> (try andP) <|> (try impliesP) <|> sentenceP

commentP :: Parser ()
commentP = optional $ do 
    string ";"
    manyTill anyChar newline
    return ()

constP :: Parser GDLExpression
constP = liftM GDLConst $ many1 $ letter <|> digit <|> char '_'

varP :: Parser GDLExpression
varP = char '?' >> (liftM GDLVar $ many letter)

notP :: Parser GDLExpression
notP = do
    char '(' >> spaces >> string "not" >> spaces
    arg <- expressionP
    spaces >> char ')'
    return $ GDLNot arg


andP :: Parser GDLExpression
andP = do
    char '(' >> spaces >> string "and" >> spaces
    arg1 <- expressionP
    spaces
    arg2 <- expressionP
    spaces >> char ')'
    return $ GDLAnd arg1 arg2


impliesP :: Parser GDLExpression
impliesP = do
    char '(' >> spaces >> string "<=" >> spaces
    conclusion <- expressionP
    spaces
    hypos <- expressionP `sepBy1` spaces
    spaces >> char ')'
    if length hypos > 1
        then return $ GDLImplies (foldr1 GDLAnd hypos) conclusion
        else return $ GDLImplies (head hypos) conclusion


sentenceP :: Parser GDLExpression
sentenceP = do
    char '(' >> spaces
    headExpr <- expressionP
    spaces
    bodyExprs <- expressionP `sepBy` spaces
    spaces >> char ')'
    return $ GDLSentence headExpr bodyExprs
