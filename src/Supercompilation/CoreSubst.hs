-- | This module is adapted from GHC's CoreSubst. We can't use it because of our
-- different Core AST type with annotations. We have to duplicate and adapt the
-- whole thing here.
module Supercompilation.CoreSubst where

import qualified Var as GHC

import Supercompilation.TaggedExpr

substTaggedExpr :: GHC.Var -> CoreExpr t -> (CoreExpr t -> CoreExpr t)
substTaggedExpr v e expr =
    substTaggedExpr' (extendSubst emptySub v e) expr
  where
    emptySub = mkEmptySub (mkInScopeSet (localFreeVarsExpr (Let undefined (NonRec v e) expr)))

substTaggedExpr' = undefined
mkEmptySub = undefined
