module Meriwether.GDL.Unification where

-- System
import GHC.Exts
import Control.Monad

-- Internal
import Meriwether.GDL.Model
import Meriwether.GDL.Substitution


-- | The class of GDL expressions that can be unified together. 
class (Show a, Eq a) => Unify a where
    -- | Unification of a and b is an operation that gives the substitutions necessary
    -- to convert a into b. It either succeeds with a list of Substitutions or fails with mzero.
    -- It is more or less true that @ substitute (unify a b) a == b @. If a is a rule, then the 
    -- conclusion of the result will be equal to b.
    unify :: (Unify b, MonadPlus m) => a -> b -> m [Substitution]
  
    -- These are a kind of hacky way to handle dynamic dispatch
    unifyTerm :: MonadPlus m => a -> Term -> m [Substitution]
    unifyTerm _ _ = mzero

    unifyAtom :: MonadPlus m => a -> Atom -> m [Substitution]
    unifyAtom _ _ = mzero

    unifyLiteral :: MonadPlus m => a -> Literal -> m [Substitution]
    unifyLiteral _ _ = mzero

    unifyRule :: MonadPlus m => a -> Rule -> m [Substitution]
    unifyRule _ _ = mzero


verifyUnification :: MonadPlus m => m [Substitution] -> m [Substitution]
verifyUnification u = do
    subs <- u
    guard $ not $ any ((> 1) . length) $ groupWith origin subs
    return subs

cleanUnification :: MonadPlus m => m [Substitution] -> m [Substitution]
cleanUnification = liftM (filter effectiveSubstitution) . verifyUnification
  where effectiveSubstitution (a :~> b) = a /= b

mergeUnifications :: MonadPlus m => [m [Substitution]] -> m [Substitution]
mergeUnifications = cleanUnification . liftM concat . sequence


-- * Instances

instance Unify Term where
    unify x y = cleanUnification $ unifyTerm y x
    unifyTerm (Const n) (Const n') = if n == n' then return [] else mzero
    unifyTerm (Function n ts) (Function n' ts')
        | (n == n') && (length ts == length ts') = mergeUnifications $ map (uncurry unify) (zip ts' ts)
        | otherwise = mzero
    unifyTerm x@(Var _) y = return [x :~> y]
    unifyTerm x@(ExistVar _) y = return [x :~> y]
    unifyTerm x y@(Var _) = return [y :~> x]
    unifyTerm x y@(ExistVar _) = return [y :~> x]
    unifyTerm _ _ = mzero

instance Unify Atom where
    unify x y = unifyAtom y x
    unifyAtom (Relation n ts) (Relation n' ts')
        | (n == n') && (length ts == length ts') = mergeUnifications $ map (uncurry unify) (zip ts' ts)
        | otherwise = mzero
    unifyAtom (Proposition n) (Proposition n') = if n == n' then return [] else mzero
    unifyAtom _ _ = mzero
    unifyRule c (c' :- ps) = unify c' c

instance Unify Literal where
    unify x y = unifyLiteral y x
    unifyLiteral (Pos a) (Pos b) = unify b a
    unifyLiteral (Neg a) (Neg b) = unify b a
    unifyLiteral (Distinct a b) (Distinct a' b') = mergeUnifications [unify a' a, unify b' b]

instance Unify Rule where
    unify x y = unifyRule y x
    unifyRule (c :- ps) (c' :- ps') = mergeUnifications $ (unify c' c):(map (uncurry unify) (zip ps' ps))
