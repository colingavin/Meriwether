-- | This module implements Substitutable for GDL expressions.
module Meriwether.GDL.Substitution where

-- System
import Data.List

-- Internal
import Meriwether.GDL.Model


-- | A Substitution replaces one Term with another.
data Substitution = (:~>) Term Term deriving Eq

instance Show Substitution where
  show (a :~> b) = (show a) ++ " :~> " ++ (show b)

-- | Give the Term that the substitution will replace.
origin :: Substitution -> Term
origin (a :~> b) = a

-- | Determines if the substitution is from a Var
isVariableSubstitution :: Substitution -> Bool
isVariableSubstitution (Var _ :~> _) = True
isVariableSubstitution _ = False

-- | Class of GDL expressions that allow substitution of terms.
class (Show a, Eq a) => Substitute a where
  variables :: a -> [Term]
  substitute :: Substitution -> a -> a

-- | Preform all substitutions in a list on an expression.
substituteAll :: Substitute a => [Substitution] -> a -> a
substituteAll = flip $ foldr substitute


-- * Instances

instance Substitute Term where
  variables v@(Var _) = [v]
  variables v@(ExistVar _) = [v]
  variables (Function _ ts) = nub $ concatMap variables ts
  variables _ = []

  substitute s (Function n ts) = Function n $ map (substitute s) ts
  substitute (a :~> b) x = if x == a then b else x

instance Substitute Atom where
  variables (Relation _ ts) = nub $ concatMap variables ts
  variables (Proposition _) = []

  substitute s (Relation n ts) = Relation n $ map (substitute s) ts
  substitute s p@(Proposition _) = p

instance Substitute Literal where
  variables (Pos a) = variables a
  variables (Neg a) = variables a
  variables (Distinct a b) = nub $ variables a ++ variables b

  substitute s (Pos a) = Pos $ substitute s a
  substitute s (Neg a) = Neg $ substitute s a
  substitute s (Distinct a b) = Distinct (substitute s a) (substitute s b)

instance Substitute Rule where
  variables (c :- ps) = nub $ variables c ++ concatMap variables ps

  substitute s (c :- ps) = (substitute s c) :- map (substitute s) ps
