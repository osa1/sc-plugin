{-# LANGUAGE Rank2Types #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
module Supercompile.Core.FreeVars (
    module Supercompile.Core.FreeVars,
    module VarSet,
    tyVarsOfType, tyVarsOfTypes, tyCoVarsOfCo
  ) where

import Supercompile.Core.Syntax
import Supercompile.Utilities

import Supercompilation.Show

import Coercion (tyCoVarsOfCo)
import CoreFVs
import CoreSyn (CoreRule (..))
import Id (idSpecialisation, isId, realIdUnfolding)
import IdInfo (specInfoRules)
import Type (tyVarsOfType, tyVarsOfTypes)
import Var (Id, TyVar)
import VarSet


type FreeVars = VarSet
type BoundVars = VarSet


varBndrFreeVars :: Var -> FreeVars
varBndrFreeVars x | isId x    = idBndrFreeVars x
                  | otherwise = tyVarBndrFreeVars x

-- We have our own version of idFreeVars so we can treat global variables as free
idBndrFreeVars :: Id -> FreeVars
idBndrFreeVars x = varTypeTyVars x `unionVarSet` -- No global tyvars, so no problem
                   rulesFreeVars (specInfoRules (idSpecialisation x)) `unionVarSet`
                   (stableUnfoldingVars (realIdUnfolding x) `orElse` emptyVarSet)
  where
    rulesFreeVars :: [CoreRule] -> VarSet
    rulesFreeVars rules = foldr (unionVarSet . ruleFreeVars) emptyVarSet rules

    ruleFreeVars :: CoreRule -> VarSet
    ruleFreeVars (BuiltinRule {}) = emptyVarSet
    ruleFreeVars (Rule { ru_fn = _, ru_bndrs = bndrs, ru_rhs = rhs, ru_args = args })
      = nonRecBindersFreeVars bndrs (exprsSomeFreeVars (const True) (rhs:args))

tyVarBndrFreeVars :: TyVar -> FreeVars
tyVarBndrFreeVars = varTypeTyVars -- No global tyvars, so no problem


(varFreeVars', termFreeVars, termFreeVars', altsFreeVars, valueFreeVars, valueFreeVars') = mkFreeVars (\f (I e) -> f e)
(fvedVarFreeVars', fvedTermFreeVars, fvedTermFreeVars', fvedAltsFreeVars, fvedValueFreeVars, fvedValueFreeVars') = mkFreeVars (\_ (FVed fvs _) -> fvs)
(sizedFVedVarFreeVars', sizedFVedTermFreeVars, sizedFVedTermFreeVars', sizedFVedAltsFreeVars, sizedFVedValueFreeVars, sizedFVedValueFreeVars') =
    mkFreeVars (\_ (Comp (Sized _ (FVed fvs _))) -> fvs)
(taggedVarFreeVars', taggedTermFreeVars, taggedTermFreeVars', taggedAltsFreeVars, taggedValueFreeVars, taggedValueFreeVars') =
    mkFreeVars (\f (Tagged _ e) -> f e)
(taggedSizedFVedVarFreeVars', taggedSizedFVedTermFreeVars, taggedSizedFVedTermFreeVars', taggedSizedFVedAltsFreeVars, taggedSizedFVedValueFreeVars, taggedSizedFVedValueFreeVars') =
    mkFreeVars (\_ (Comp (Tagged _ (Comp (Sized _ (FVed fvs _))))) -> fvs)

{-# INLINE mkFreeVars #-}
mkFreeVars :: (forall a. (a -> FreeVars) -> ann a -> FreeVars)
           -> (Var              -> FreeVars,
               ann (TermF ann)  -> FreeVars,
               TermF ann        -> FreeVars,
               [AltF ann]       -> FreeVars,
               ann (ValueF ann) -> FreeVars,
               ValueF ann       -> FreeVars)
mkFreeVars rec = (unitVarSet, term, term', alternatives, value, value')
  where
    term = rec term'
    term' (Var x)            = unitVarSet x
    term' (Value v)          = value' v
    term' (TyApp e ty)       = tyVarsOfType ty `unionVarSet` term e
    term' (CoApp e co)       = term e `unionVarSet` tyCoVarsOfCo co
    term' (App e x)          = term e `extendVarSet` x
    term' (PrimOp _ tys es)  = unionVarSets (map tyVarsOfType tys) `unionVarSet` unionVarSets (map term es)
    term' (Case e x ty alts) = tyVarsOfType ty `unionVarSet` term e `unionVarSet` nonRecBinderFreeVars x (alternatives alts)
    term' (Let x e1 e2)      = term e1 `unionVarSet` nonRecBinderFreeVars x (term e2)
    term' (LetRec xes e)     = (unionVarSets (map term es) `unionVarSet` term e `unionVarSet` unionVarSets (map idBndrFreeVars xs)) `delVarSetList` xs
      where (xs, es) = unzip xes
    term' (Cast e co)        = term e `unionVarSet` tyCoVarsOfCo co

    value = rec value'
    value' = valueGFreeVars' term

    alternatives = unionVarSets . map alternative

    alternative (altcon, e) = altConFreeVars altcon $ term e

valueGFreeVars' :: (a -> FreeVars) -> ValueG a -> FreeVars
valueGFreeVars' term (TyLambda x e)      = nonRecBinderFreeVars x (term e)
valueGFreeVars' term (Lambda x e)        = nonRecBinderFreeVars x (term e)
valueGFreeVars' _    (Data _ tys cos xs) = unionVarSets (map tyVarsOfType tys) `unionVarSet` unionVarSets (map tyCoVarsOfCo cos) `unionVarSet` mkVarSet xs
valueGFreeVars' _    (Literal _)         = emptyVarSet
valueGFreeVars' _    (Coercion co)       = tyCoVarsOfCo co

nonRecBinderFreeVars :: Var -> FreeVars -> FreeVars
nonRecBinderFreeVars x fvs = (fvs `delVarSet` x) `unionVarSet` varBndrFreeVars x

nonRecBindersFreeVars :: [Var] -> FreeVars -> FreeVars
nonRecBindersFreeVars xs = flip (foldr nonRecBinderFreeVars) xs

-- Returns the most tightly binding variable last
altConBoundVars :: AltCon -> [Var]
altConBoundVars (DataAlt _ as qs xs) = as ++ qs ++ xs
altConBoundVars (LiteralAlt _)       = []
altConBoundVars _                    = []

altConFreeVars :: AltCon -> FreeVars -> FreeVars
altConFreeVars (DataAlt _ as qs xs) = (`delVarSetList` as) . nonRecBindersFreeVars (qs ++ xs)
altConFreeVars (LiteralAlt _)       = id
altConFreeVars DefaultAlt           = id


coercedFreeVars :: (a -> FreeVars) -> Coerced a -> FreeVars
coercedFreeVars f (cast_by, x) = f x `unionVarSet` castByFreeVars cast_by

castByFreeVars :: CastBy -> FreeVars
castByFreeVars Uncast        = emptyVarSet
castByFreeVars (CastBy co _) = tyCoVarsOfCo co


data FVed a = FVed { freeVars :: !FreeVars, fvee :: !a }

instance Copointed FVed where
    extract = fvee

instance Functor FVed where
    fmap f (FVed fvs x) = FVed fvs (f x)

instance Foldable FVed where
    foldMap f (FVed _ x) = f x

instance Traversable FVed where
    traverse f (FVed fvs x) = pure (FVed fvs) <*> f x

instance Show1 FVed where
    showsPrec1 prec (FVed fvs x) = showParen (prec >= appPrec) (showString "FVed" . showsPrec appPrec (varSetElems fvs) . showsPrec appPrec x)

instance Eq1 FVed where
    eq1 (FVed fvs1 x1) (FVed fvs2 x2) = varSetElems fvs1 == varSetElems fvs2 && x1 == x2

instance Ord1 FVed where
    compare1 (FVed fvs1 x1) (FVed fvs2 x2) = (x1, varSetElems fvs1) `compare` (x2, varSetElems fvs2)

instance Outputable1 FVed where
    pprPrec1 prec (FVed _ x) = pprPrec prec x

instance OutputableLambdas1 FVed where
    pprPrecLam1 (FVed _ x) = pprPrecLam x

instance Show a => Show (FVed a) where
    showsPrec = showsPrec1

instance Eq a => Eq (FVed a) where
    (==) = eq1

instance Ord a => Ord (FVed a) where
    compare = compare1

instance Outputable a => Outputable (FVed a) where
    pprPrec = pprPrec1


type FVedTerm = FVed (TermF FVed)
type FVedAlt = AltF FVed
type FVedValue = ValueF FVed


instance Symantics FVed where
    var = fvedTerm . Var
    value = fmap Value . fvedValue
    tyApp e = fvedTerm . TyApp e
    coApp e = fvedTerm . CoApp e
    app e = fvedTerm . App e
    primOp pop tys = fvedTerm . PrimOp pop tys
    case_ e x ty = fvedTerm . Case e x ty
    let_ x e1 = fvedTerm . Let x e1
    letRec xes = fvedTerm . LetRec xes
    cast e = fvedTerm . Cast e

fvedVar :: Var -> FVed Var
fvedVar x = FVed (taggedVarFreeVars' x) x

fvedValue :: ValueF FVed -> FVed FVedValue
fvedValue v = FVed (fvedValueFreeVars' v) v

fvedTerm :: TermF FVed -> FVedTerm
fvedTerm e = FVed (fvedTermFreeVars' e) e
