module Meriwether.GDL.Model where

data GDLExpression = GDLConst       String
                   | GDLVar         String
                   | GDLNot         GDLExpression
                   | GDLAnd         GDLExpression GDLExpression
                   | GDLImplies     {getPremise :: GDLExpression, getConclusion :: GDLExpression}
                   | GDLSentence    {getHead :: GDLExpression, getBody :: [GDLExpression]}
    deriving (Show, Eq)

type GDLDocument = [GDLExpression]
