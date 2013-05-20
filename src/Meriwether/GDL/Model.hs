module Meriwether.GDL.Model where

-- System
import Data.List

type Name = String

data Term = Var Name | ExistVar Name | Const Name | Function Name [Term] deriving (Eq, Ord)

data Atom = Relation Name [Term] | Proposition Name deriving (Eq)

data Literal = Pos Atom | Neg Atom | Distinct Term Term deriving (Eq)

data Rule = (:-) Atom [Literal] deriving (Eq)

data Expression = Ground Atom | InferenceRule Rule deriving (Eq, Show)

-- * Show instances

instance Show Term where
  show (Var n) = "?" ++ n
  show (ExistVar n) = "*" ++ n
  show (Const n) = n
  show (Function n ts) = "(" ++ n ++ " " ++ (intercalate " " $ map show ts) ++ ")"

instance Show Atom where
  show (Relation n ts) = "(" ++ n ++ " " ++ (intercalate " " $ map show ts) ++ ")"
  show (Proposition name) = name

instance Show Literal where
  show (Pos r) = (show r)
  show (Neg r) = "(not " ++ (show r) ++ ")"
  show (Distinct a b) = "(distinct " ++ (show a) ++ " " ++ (show b) ++ ")"

instance Show Rule where
  show (c :- ps) = "(" ++ (show c) ++ " <= " ++ (intercalate " " $ map show ps) ++ ")"
