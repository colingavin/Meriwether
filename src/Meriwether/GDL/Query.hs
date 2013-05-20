{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Meriwether.GDL.Query where

-- System
import Data.List
import Debug.Trace

-- External
import Control.Monad.Logic
import Control.Monad.Reader

-- Internal
import Meriwether.GDL.Model
import Meriwether.GDL.Substitution
import Meriwether.GDL.Unification


-- | Deductive systems are the basic representation of GDL systems.
newtype DeductiveSystem = DeductiveSystem [Rule] deriving (Show, Eq)

normalize :: Expression -> Rule
normalize (Ground atom) = atom :- [] -- All atoms are turned into zero-predicate rules
normalize (InferenceRule (c :- ps)) = c :- map (substituteAll subs) ps
  where existVars = (concat $ map variables ps) \\ (variables c)
        makeSub v@(Var n) = v :~> ExistVar n
        makeSub v@(ExistVar n) = v :~> v
        subs = map makeSub existVars

-- | Constructor for deductive systems. Normalizes each element of the list of literals.
deductiveSystem :: [Expression] -> DeductiveSystem
deductiveSystem = DeductiveSystem . map normalize

systemRules :: DeductiveSystem -> Logic Rule
systemRules (DeductiveSystem rs) = msum (map return rs)

-- | This is a Monad used to run queries
newtype Query a = Query {
  runQuery :: ReaderT [Substitution] Logic a
} deriving (Monad, MonadPlus, MonadLogic, MonadReader [Substitution])

-- | Use to bring a Logic object into a Query computation.
liftQuery :: Logic a -> Query a
liftQuery = Query . lift

-- | Utility to extract a Logic computation from a Query.
descendQuery :: Query a -> Logic a
descendQuery s = runReaderT (runQuery s) []

-- | Extracts at most n results from the query.
observeManyQuery :: Int -> Query a -> [a]
observeManyQuery n = (observeMany n) . descendQuery

-- | Extracts all the results from the query.
observeAllQuery :: Query a -> [a]
observeAllQuery = observeAll . descendQuery

-- | Extracts one result from the query and fails if there are no results.
observeQuery :: Query a -> a
observeQuery = observe . descendQuery

-- | Expressions are queryable if they can be queried within a deductive system.
class (Show a) => Queryable a where
  query :: a -> DeductiveSystem -> Query [Substitution]
  
  prove :: a -> DeductiveSystem -> Query Bool
  prove a ds = ifte (query a ds) (const $ return True) (return False)

-- * Implementation Utilities
choose :: [a] -> Logic a
choose = msum . (map return)

-- * Instances

instance Queryable Atom where
  query a ds@(DeductiveSystem rs) = do
    -- Choose a rule to try to satisfy
    rule <- liftQuery $ choose rs
    -- Extract a set of substitutions into the rule (type spec is necessary to get it into the logic monad)
    subs <- liftQuery $ unify rule a
    let newRule = substituteAll subs rule
    -- Query the rule with the new substitutions
    local (++subs) $ query newRule ds


instance Queryable Rule where
  -- If the predicate list is empty, the conclusion is true, return the substitutions that got us here
  -- without substitutions of free variables
  query (_ :- []) _ = filter isVariableSubstitution `liftM` ask

  -- Otherwise, query the first predicate
  query (c :- ps) ds@(DeductiveSystem rs) = do
    -- Get a substitution into that makes the first predicate true and is compatible with the current subs
    newSubs <- mergeUnifications [query (head ps) ds, ask]
    -- Query for the rest of the predicates
    local (const newSubs) $ query (c :- tail ps) ds


instance Queryable Literal where
  -- To query a positive atom, just query the atom
  query (Pos a) ds = query a ds

  -- To query a negative atom, see if there are any substitutions that make it true
  -- If there are, the query is unsuccessful, otherwise, return the current substitutions
  query (Neg a) ds = ifte (query a ds) (const mzero) ask

  -- TODO: Implement the distinct literal
  query (Distinct a b) ds = undefined
