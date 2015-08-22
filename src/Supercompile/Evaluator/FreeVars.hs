module Supercompile.Evaluator.FreeVars (
    inFreeVars,
    heapBindingFreeVars,
    pureHeapBoundVars, stackBoundVars, stackFreeVars, stackOpenFreeVars, stackFrameBoundVars, stackFrameFreeVars,
    qaFreeVars, pureHeapVars,
    unnormalisedStateFreeVars, unnormalisedStateUncoveredVars,
    stateFreeVars, stateAllFreeVars, stateLetBounders, stateLambdaBounders, stateInternalBounders, stateUncoveredVars,
    module Supercompile.Core.FreeVars
  ) where

import Supercompile.Evaluator.Syntax

import Supercompile.Core.FreeVars
import Supercompile.Core.Renaming

import Supercompile.Utilities

import qualified Data.Map as M


-- | Finds the set of things "referenced" by a 'HeapBinding': this is only used to construct tag-graphs
heapBindingFreeVars :: HeapBinding -> FreeVars
heapBindingFreeVars = maybe emptyVarSet (inFreeVars annedTermFreeVars) . heapBindingTerm

-- | Returns all the variables bound by the heap that we might have to residualise in the splitter
pureHeapBoundVars :: PureHeap -> BoundVars
pureHeapBoundVars = mkVarSet . M.keys -- I think its harmless to include variables bound by phantoms in this set

-- | Returns all the variables bound by the stack that we might have to residualise in the splitter
stackBoundVars :: Stack -> BoundVars
stackBoundVars = fst . stackOpenFreeVars

-- | Returns all the variables referred to by the stack, even ones also bound by the stack
stackFreeVars :: Stack -> FreeVars
stackFreeVars = snd . stackOpenFreeVars

stackOpenFreeVars :: Stack -> (BoundVars, FreeVars)
stackOpenFreeVars = (unionVarSets *** unionVarSets) . unzip . map (stackFrameOpenFreeVars . tagee) . trainCars

stackFrameBoundVars :: StackFrame -> BoundVars
stackFrameBoundVars = fst . stackFrameOpenFreeVars

stackFrameFreeVars :: StackFrame -> FreeVars
stackFrameFreeVars = snd . stackFrameOpenFreeVars

stackFrameOpenFreeVars :: StackFrame -> (BoundVars, FreeVars)
stackFrameOpenFreeVars kf = case kf of
    TyApply ty'              -> (emptyVarSet, tyVarsOfType ty')
    CoApply co'              -> (emptyVarSet, tyCoVarsOfCo co')
    Apply x'                 -> (emptyVarSet, unitVarSet x')
    Scrutinise x' ty in_alts -> (emptyVarSet, (nonRecBinderFreeVars x' (inFreeVars annedAltsFreeVars in_alts)) `unionVarSet` tyVarsOfType ty)
    PrimApply _ tys as in_es -> (emptyVarSet, unionVarSets (map tyVarsOfType tys) `unionVarSet` unionVarSets (map annedFreeVars as) `unionVarSet` unionVarSets (map (inFreeVars annedTermFreeVars) in_es))
    StrictLet x' in_e2       -> (emptyVarSet, nonRecBinderFreeVars x' (inFreeVars annedTermFreeVars in_e2))
    Update x'                -> (unitVarSet x', idBndrFreeVars x')
    CastIt co'               -> (emptyVarSet, tyCoVarsOfCo co')


-- | Computes the variables bound and free in a state
unnormalisedStateVars :: UnnormalisedState -> (HowBound -> BoundVars, FreeVars)
stateVars :: State -> (HowBound -> BoundVars, FreeVars)
pureHeapVars :: PureHeap -> (HowBound -> BoundVars, FreeVars)
(unnormalisedStateVars, stateVars, pureHeapVars)
  = (\(_, Heap h _, k, in_e) -> finish $ pureHeapOpenFreeVars h (stackOpenFreeVars' k (inFreeVars annedFreeVars in_e)),
     \(_, Heap h _, k, a)    -> finish $ pureHeapOpenFreeVars h (stackOpenFreeVars' k (annedFreeVars a)),
     \h -> finish $ pureHeapOpenFreeVars h (emptyVarSet, emptyVarSet))
  where
    finish ((bvs_internal, bvs_lambda, bvs_let), fvs) = (\how -> case how of InternallyBound -> bvs_internal; LambdaBound -> bvs_lambda; LetBound -> bvs_let, fvs)
    
    pureHeapOpenFreeVars :: PureHeap -> (BoundVars, FreeVars) -> ((BoundVars, BoundVars, BoundVars), FreeVars)
    pureHeapOpenFreeVars h (bvs_internal, fvs) = (\f -> M.foldrWithKey f ((bvs_internal, emptyVarSet, emptyVarSet), fvs) h) $ \x' hb ((bvs_internal, bvs_lambda, bvs_let), fvs) -> (case howBound hb of
        InternallyBound -> (bvs_internal `extendVarSet` x', bvs_lambda, bvs_let)
        LambdaBound     -> (bvs_internal, bvs_lambda `extendVarSet` x', bvs_let)
        LetBound        -> (bvs_internal, bvs_lambda, bvs_let `extendVarSet` x'),
        fvs `unionVarSet` varBndrFreeVars x' `unionVarSet` heapBindingFreeVars hb)
    
    stackOpenFreeVars' :: Stack -> FreeVars -> (BoundVars, FreeVars)
    stackOpenFreeVars' k fvs = case stackOpenFreeVars k of (k_bvs, k_fvs) -> (k_bvs, fvs `unionVarSet` k_fvs)


qaFreeVars :: QA -> FreeVars
qaFreeVars (Question x') = unitVarSet x'
qaFreeVars (Answer a)    = answerFreeVars' a


-- | Returns (an overapproximation of) the free variables that the state would have if it were residualised right now (i.e. variables bound by phantom bindings *are* in the free vars set)
stateFreeVars :: State -> FreeVars
stateFreeVars s = fvs `minusVarSet` bvs InternallyBound
  where (bvs, fvs) = stateVars s

unnormalisedStateFreeVars :: UnnormalisedState -> FreeVars
unnormalisedStateFreeVars s = fvs `minusVarSet` bvs InternallyBound
  where (bvs, fvs) = unnormalisedStateVars s

unnormalisedStateUncoveredVars :: UnnormalisedState -> FreeVars
unnormalisedStateUncoveredVars s = fvs `minusVarSet` bvs InternallyBound `minusVarSet` bvs LetBound `minusVarSet` bvs LambdaBound
  where (bvs, fvs) = unnormalisedStateVars s


stateAllFreeVars :: State -> FreeVars
stateAllFreeVars = snd . stateVars

stateLetBounders :: State -> BoundVars
stateLetBounders = ($ LetBound) . fst . stateVars

stateLambdaBounders :: State -> BoundVars
stateLambdaBounders = ($ LambdaBound) . fst . stateVars

stateInternalBounders :: State -> BoundVars
stateInternalBounders = ($ InternallyBound) . fst . stateVars

stateUncoveredVars :: State -> FreeVars
stateUncoveredVars s = fvs `minusVarSet` bvs InternallyBound `minusVarSet` bvs LetBound `minusVarSet` bvs LambdaBound
  where (bvs, fvs) = stateVars s
