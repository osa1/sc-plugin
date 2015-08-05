-- | We duplicate the whole CoreExpr here, just to be able to annotate it with
-- tags. This also means we need to duplicate CoreExpr functions.
module Supercompilation.TaggedExpr where

import qualified Coercion as GHC
import qualified CoreSyn as GHC
import qualified Literal as GHC
import qualified TypeRep as GHC
import qualified Var as GHC

data Expr b t
    -- NB. I always confuse this: Id = Var in GHC
  = Var   GHC.Var
  | Lit   GHC.Literal
  | App   (Expr b t) (Expr b t)
  | Lam   b (Expr b t)
  | Let   (Bind b t) (Expr b t)
  | Case  (Expr b t) b GHC.Type [Alt b t]
  | Cast  (Expr b t) GHC.Coercion
  | Tick  (GHC.Tickish GHC.Id) (Expr b t)
  | Type  GHC.Type
  | Coercion GHC.Coercion

data Bind b t
  = NonRec b (Expr b t)
  | Rec [(b,  Expr b t)]

type Alt b t = (GHC.AltCon, [b], Expr b t)
