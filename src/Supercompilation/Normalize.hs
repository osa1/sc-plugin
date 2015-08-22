{-# LANGUAGE FlexibleContexts #-}

module Supercompilation.Normalize where

import Data.Maybe (isJust, isNothing)

import Id

import Supercompilation.Context as C
import Supercompilation.Show ()
import Supercompilation.TaggedExpr as E

--------------------------------------------------------------------------------

normalize :: UState -> State
normalize (heap, expr, stack) = normalize' heap expr stack

normalize' :: Heap -> TaggedCoreExpr -> Stack -> State

-- UPDATEV
normalize' h e ((t, Update v) : s)
  -- NB. We may want to use tag of e in heap binding
  | isValue e = normalize' (heapInsert v e h) (Var t v) s

-- UPDATE
normalize' h var@(Var _ v0) ((_, Update v) : s)
  | isJust (heapLookup v0 h) = normalize' (heapInsert v var h) var s

-- VAR
normalize' h (Var t v) s
  | Just expr <- heapLookup v h = normalize' (heapRemove v h) expr ((t, Update v) : s)

-- APP
normalize' h (App t f a) s = normalize' h f ((t, Supply a) : s)

-- BETAV
normalize' h (Lam t a' body) ((_, Supply a) : s) =
    -- which tag should we use here?
    -- TODO: we need renaming here, in case a' is free in a
    normalize' h (Let t (NonRec a' a) body) s

-- CASE
normalize' h (E.Case t e b ty alts) s = normalize' h e ((t, C.Case b ty alts) : s)

-- normalize' h e ((t, C.Case b ty alts) : s)
--   | Just (con, ty_args, other_args) <- exprIsConApp_maybe (getUnfoldingInRuleMatch env) scrut
--   =

--------------------------------------------------------------------------------

isValue :: TaggedCoreExpr -> Bool
isValue Lam{} = True
isValue Type{} =
    -- this is not true but let'e leave it this way until I understand why we need
    -- type level normalization
    True
isValue (App _ (Var _ v) _) = isJust (isDataConId_maybe v)
isValue _ = False

deref :: Heap -> TaggedCoreExpr -> TaggedCoreExpr
deref h (Var _ v)
  | Just t <- heapLookup v h = t
deref _ t = t

-- | According to Theorem 2.2.2, normalized states are guaranteed to have this
-- form.
isNormalized :: UState -> Bool

isNormalized (h, t, [])
  -- value
  | isValue (deref h t) = True

isNormalized (h, Var _ v, _)
  -- free-variable reference
  | isNothing (heapLookup v h) = True

isNormalized (h, Var _ v, (_, Supply{}) : _)
  | Just Lam{}  <- heapLookup v h = True
  | Just Type{} <- heapLookup v h = True

isNormalized _ = False
