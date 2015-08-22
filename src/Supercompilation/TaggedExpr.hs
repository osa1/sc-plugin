{-# LANGUAGE TupleSections #-}

-- | We duplicate the whole CoreExpr here, just to be able to annotate it with
-- tags. This also means we need to duplicate CoreExpr functions.
module Supercompilation.TaggedExpr where

import Control.Monad.State
import Data.Bifunctor (second)

import qualified Coercion as GHC
import qualified CoreFVs as GHC
import qualified CoreSyn as GHC
import qualified Literal as GHC
import qualified TypeRep as GHC
import qualified Var as GHC
import qualified VarSet as GHC

--------------------------------------------------------------------------------

type CoreProgram t = [CoreBind t]
type CoreBndr      = GHC.Var
type CoreBind t    = Bind CoreBndr t
type CoreExpr t    = Expr CoreBndr t
type CoreAlt t     = Alt CoreBndr t

type TaggedCoreProgram = CoreProgram Tag
type TaggedCoreBind    = CoreBind Tag
type TaggedCoreExpr    = CoreExpr Tag
type TaggedCoreAlt     = CoreAlt Tag

-- TODO: Do I need to tag types and coercions?

data Expr b t
    -- NB. I always confuse this: Id = Var in GHC
  = Var   t GHC.Var
  | Lit   t GHC.Literal
  | App   t (Expr b t) (Expr b t)
  | Lam   t b (Expr b t)
  | Let   t (Bind b t) (Expr b t)
  | Case  t (Expr b t) b GHC.Type [Alt b t]
  | Cast  t (Expr b t) GHC.Coercion
  | Tick  t (GHC.Tickish GHC.Id) (Expr b t)
  | Type  t GHC.Type
  | Coercion t GHC.Coercion

data Bind b t
  = NonRec b (Expr b t)
  | Rec [(b,  Expr b t)]

type Alt b t = (GHC.AltCon, [b], Expr b t)

--------------------------------------------------------------------------------
-- * Tagging CorePrograms

type Tag = Int

tagPgm :: GHC.CoreProgram -> TaggedCoreProgram
tagPgm pgm = evalState (mapM tagBind pgm) 0

tagBind :: GHC.CoreBind -> State Tag TaggedCoreBind
tagBind (GHC.NonRec b e) = NonRec b <$> tagExpr e
tagBind (GHC.Rec bs)     = Rec <$> mapM tagBind' bs

tagBind' :: (GHC.CoreBndr, GHC.CoreExpr) -> State Tag (CoreBndr, TaggedCoreExpr)
tagBind' (b, e) = (b,) <$> tagExpr e

tagExpr :: GHC.CoreExpr -> State Tag TaggedCoreExpr
tagExpr (GHC.Var v)         =
    freshTag >>= \t -> return (Var t v)
tagExpr (GHC.Lit lit)       =
    freshTag >>= \t -> return (Lit t lit)
tagExpr (GHC.App f arg)     =
    freshTag >>= \t -> App t <$> tagExpr f <*> tagExpr arg
tagExpr (GHC.Lam arg body)  =
    freshTag >>= \t -> Lam t arg <$> tagExpr body
tagExpr (GHC.Let b body)    =
    freshTag >>= \t -> Let t <$> tagBind b <*> tagExpr body
tagExpr (GHC.Case scrt b ty alts) =
    freshTag >>= \t -> Case t <$> tagExpr scrt <*> pure b <*> pure ty <*> mapM tagAlt alts
tagExpr (GHC.Cast e c)      =
    freshTag >>= \t -> Cast t <$> tagExpr e <*> pure c
tagExpr (GHC.Tick tick e)   =
    freshTag >>= \t -> Tick t tick <$> tagExpr e
tagExpr (GHC.Type ty)       =
    freshTag >>= \t -> return (Type t ty)
tagExpr (GHC.Coercion c)    =
    flip Coercion c <$> freshTag

tagAlt :: GHC.CoreAlt -> State Tag TaggedCoreAlt
tagAlt (con, bs, e) = (con, bs,) <$> tagExpr e

freshTag :: State Tag Tag
freshTag = do
    t <- get
    modify (+ 1)
    return t

--------------------------------------------------------------------------------
-- * Untagging CorePrograms

untagPgm :: CoreProgram t -> GHC.CoreProgram
untagPgm bs = map untagBind bs

untagBind :: CoreBind t -> GHC.CoreBind
untagBind (NonRec b e) = GHC.NonRec b $ untagExpr e
untagBind (Rec bs)     = GHC.Rec $ map (second untagExpr) bs

untagExpr :: CoreExpr t -> GHC.CoreExpr
untagExpr (Var _ v) = GHC.Var v
untagExpr (Lit _ l) = GHC.Lit l
untagExpr (App _ f a) = GHC.App (untagExpr f) (untagExpr a)
untagExpr (Lam _ b e) = GHC.Lam b (untagExpr e)
untagExpr (Let _ b e) = GHC.Let (untagBind b) (untagExpr e)
untagExpr (Case _ e b t as) = GHC.Case (untagExpr e) b t (map untagAlt as)
untagExpr (Cast _ e c) = GHC.Cast (untagExpr e) c
untagExpr (Tick _ t e) = GHC.Tick t (untagExpr e)
untagExpr (Type _ t) = GHC.Type t
untagExpr (Coercion _ c) = GHC.Coercion c

untagAlt :: CoreAlt t -> GHC.CoreAlt
untagAlt (con, bs, e) = (con, bs, untagExpr e)

--------------------------------------------------------------------------------
-- * Extracting tags from terms

exprTag :: TaggedCoreExpr -> Tag
exprTag (Var t _) = t
exprTag (Lit t _) = t
exprTag (App t _ _) = t
exprTag (Lam t _ _) = t
exprTag (Let t _ _) = t
exprTag (Case t _ _ _ _) = t
exprTag (Cast t _ _) = t
exprTag (Tick t _ _) = t
exprTag (Type t _) = t
exprTag (Coercion t _) = t

--------------------------------------------------------------------------------
-- * Free variables in tagged Core expressions

-- |
-- TODO: This code is adapted from HERMIT, make a note of this somewhere.
--       (also has some parts from GHC)
freeVarsExpr :: CoreExpr t -> GHC.VarSet
freeVarsExpr (Var _ v) = GHC.extendVarSet (freeVarsVar v) v
freeVarsExpr Lit{} = GHC.emptyVarSet
freeVarsExpr (App _ e1 e2) = freeVarsExpr e1 `GHC.unionVarSet` freeVarsExpr e2
freeVarsExpr (Lam _ b e) = GHC.delVarSet (freeVarsExpr e) b
freeVarsExpr (Let _ b e) =
    freeVarsBind b `GHC.unionVarSet` GHC.delVarSetList (freeVarsExpr e) (GHC.bindersOf $ untagBind b)
freeVarsExpr (Case _ s b ty alts) =
    let altFVs = GHC.delVarSet (GHC.unionVarSets $ map freeVarsAlt alts) b
     in GHC.unionVarSets [freeVarsExpr s, freeVarsType ty, altFVs]
freeVarsExpr (Cast _ e co) = freeVarsExpr e `GHC.unionVarSet` freeVarsCoercion co
freeVarsExpr (Tick _ t e) = freeVarsTick t `GHC.unionVarSet` freeVarsExpr e
freeVarsExpr (Type _ ty) = freeVarsType ty
freeVarsExpr (Coercion _ co) = freeVarsCoercion co

freeVarsAlt :: CoreAlt t -> GHC.VarSet
freeVarsAlt (_, bs, e) =
    GHC.delVarSetList (freeVarsExpr e `GHC.unionVarSet` GHC.unionVarSets (map freeVarsVar bs)) bs

freeVarsCoercion :: GHC.Coercion -> GHC.VarSet
freeVarsCoercion = GHC.tyCoVarsOfCo

freeVarsType :: GHC.Type -> GHC.TyVarSet
freeVarsType = GHC.tyVarsOfType

freeVarsVar :: GHC.Var -> GHC.VarSet
freeVarsVar v = GHC.varTypeTyVars v `GHC.unionVarSet` bndrRuleAndUnfoldingVars
  where
    -- This function is copied from GHC, which defines but doesn't expose it.
    --
    -- A 'let' can bind a type variable, and idRuleVars assumes it's seeing an
    -- Id. This function tests first.
    bndrRuleAndUnfoldingVars :: GHC.VarSet
    bndrRuleAndUnfoldingVars
      | GHC.isTyVar v = GHC.emptyVarSet
      | otherwise     = GHC.idRuleAndUnfoldingVars v

freeVarsBind :: CoreBind t -> GHC.VarSet
freeVarsBind (NonRec v e) = freeVarsExpr e `GHC.unionVarSet` freeVarsVar v
freeVarsBind (Rec defs)   =
    let (bs, es) = unzip defs
     in GHC.delVarSetList (GHC.unionVarSets (map freeVarsVar bs ++ map freeVarsExpr es)) bs

freeVarsTick :: GHC.Tickish GHC.Id -> GHC.VarSet
freeVarsTick (GHC.Breakpoint _ ids) = GHC.mkVarSet ids
freeVarsTick _ = GHC.emptyVarSet

--------------------------------------------------------------------------------
-- * Capture avoiding substitutions


