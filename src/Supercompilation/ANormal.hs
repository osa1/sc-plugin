{-# LANGUAGE TupleSections #-}

-- | This is a Core-to-Core pass that does transforms Core programs to A-normal
-- form.
module Supercompilation.ANormal where

-- import qualified Data.Set as S
import Control.Monad.State

import CoreSyn
import CoreUtils
import FastString hiding (uniq) -- Some of the fields of FastString record are
                                -- prefixed with fs_, but uniq is not.
import IdInfo
import Name
import SrcLoc
import Unique
import Var

-- A state monad is not really the best option here, but works fine here.
-- (We do some redundant updated-state passing here.)

-- TODO: We generate prefixed names but we don't guarantee that there won't be
-- shadowing/capturing.
-- This may be a simple solution: Some names are not allowed to be used by users
-- but GHC can still generate those, like $d prefixed dictionary names. Maybe we
-- can use something like $a for A-normal form transformation generated names.

aNormalPgm :: CoreProgram -> CoreProgram
aNormalPgm pgm = evalState (mapM aNormalBind pgm) (mkVarOccUnique $ mkFastString "an_")

aNormalBind :: CoreBind -> State Unique CoreBind
aNormalBind (NonRec b e) = NonRec b <$> aNormal e
aNormalBind (Rec bs) = Rec <$> mapM aNormalBind' bs

aNormalBind' :: (CoreBndr, CoreExpr) -> State Unique (CoreBndr, CoreExpr)
aNormalBind' (b, e) = (b,) <$> aNormal e

-- TODO: Complete implementation.
aNormal :: CoreExpr -> State Unique CoreExpr
aNormal e@Var{} = return e
aNormal e@Lit{} = return e
aNormal (App f arg) = do
    arg' <- aNormal arg
    fv <- freshVarForExpr arg
    return $ Let (NonRec fv arg') (App f (Var fv))
aNormal (Lam arg body) = Lam arg <$> aNormal body
aNormal (Let bs body) = Let <$> aNormalBind bs <*> aNormal body
aNormal (Case e b t alts) = do
    e' <- aNormal e
    alts' <- mapM aNormalAlt alts
    return $ Case e' b t alts'
aNormal (Cast e c) = flip Cast c <$> aNormal e
aNormal (Tick t e) = Tick t <$> aNormal e
aNormal t@Type{} = return t
aNormal c@Coercion{} = return c

aNormalAlt :: CoreAlt -> State Unique CoreAlt
aNormalAlt (con, bs, e) = (con, bs,) <$> aNormal e

freshVarForExpr :: CoreExpr -> State Unique Var
freshVarForExpr expr = do
    uniq <- get
    modify incrUnique
    let name     = mkInternalName uniq (mkVarOccFS $ mkFastString "an") noSrcSpan
        freshVar = mkLocalVar VanillaId name (exprType expr) vanillaIdInfo
    return freshVar
