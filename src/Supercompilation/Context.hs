{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Supercompilation.Context where

import Control.Monad.State.Strict as S
import qualified Data.IntMap as IM

import DynFlags
import HscTypes (ModGuts)
import Type
import Unique

import Supercompilation.TaggedExpr

type State  = (Heap, QA            , Stack)
type UState = (Heap, TaggedCoreExpr, Stack)

type Heap = IM.IntMap TaggedCoreExpr

-- NOTE: We may need to use CoreExpr for both cases, and use some smart
-- constructor functions.
data QA = -- Question Var | Answer Value
          Question Var | Answer TaggedCoreExpr

type Stack = [StackFrame]

type StackFrame = (Tag, UStackFrame)

data UStackFrame
  = Update Var -- Type     -- TODO: Do we really need a Type here?
  | Supply TaggedCoreExpr  -- ^ Supply the argument to function value
  | Instantiate Type       -- ^ Instantiate value
  | Case CoreBndr Type [TaggedCoreAlt]      -- TODO: Is using CoreAlt here a good idea?

data ScpState = ScpState
  { ssGuts     :: ModGuts
  , ssDynFlags :: DynFlags
  }

newtype ScpM a = ScpM { unwrapScpM :: S.State ScpState a }
  deriving (Functor, Applicative, Monad, S.MonadState ScpState)

instance HasDynFlags ScpM where
  getDynFlags = S.gets ssDynFlags

heapLookup :: Var -> Heap -> Maybe TaggedCoreExpr
heapLookup v h = IM.lookup (getKey . getUnique $ v) h

heapRemove :: Var -> Heap -> Heap
heapRemove v h = IM.delete (getKey . getUnique $ v) h

heapInsert :: Var -> TaggedCoreExpr -> Heap -> Heap
heapInsert v = IM.insert (getKey . getUnique $ v)
