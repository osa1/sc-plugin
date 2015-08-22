{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
module Supercompile.Drive.Split (
    MonadStatics(..), split, instanceSplit, generalise,
    ResidTags, plusResidTags, emptyResidTags) where

import Supercompile.Core.FreeVars
import Supercompile.Core.Renaming
import Supercompile.Core.Syntax

import Supercompile.Evaluator.Deeds
import Supercompile.Evaluator.Evaluate (normalise)
import Supercompile.Evaluator.FreeVars
import Supercompile.Evaluator.Residualise
import Supercompile.Evaluator.Syntax

import Supercompile.GHC (coreAltConToAltCon, altConToCoreAltCon)

import Supercompile.Termination.Generaliser (Generaliser(..))

import Supercompile.StaticFlags
import Supercompile.Utilities hiding (tails)

import CoreUtils (filterAlts)
import Id        (idUnique, idType, isDeadBinder, localiseId, isOneShotBndr)
import Var       (varUnique)
import PrelNames (undefinedName, wildCardKey)
import Util      (zipWithEqual, zipWith3Equal, thirdOf3)
import Digraph
import UniqFM    (delListFromUFM_Directly)
import VarEnv

import Data.Traversable (fmapDefault, foldMapDefault)
import qualified Data.Map as M
import qualified Data.IntSet as IS
import qualified Data.IntMap as IM


class Monad m => MonadStatics m where
    bindCapturedFloats :: FreeVars -> m a -> m (Out [(Var, FVedTerm)], a)
    -- Free variables of h-functions generated in this context
    monitorFVs :: m a -> m (FreeVars, a)


-- | Implementation of Kleene fixed-point theorem <http://en.wikipedia.org/wiki/Kleene_fixed-point_theorem>
-- for bounded join semi-lattices. Forces the function to be monotone.
{-# INLINE lfpFrom #-}
lfpFrom :: (Eq a) => (a -> a -> a) -> a -> (a -> a) -> a
lfpFrom join init_x f = lfpFrom' init_x (\x -> f x `join` x)
  where
    -- Least point of a partially ordered monotone function. Does not checks that the function is monotone.
    lfpFrom' init_x f = go init_x
      where go x | x' == x      = x
                 | otherwise    = go x'
              where x' = f x


type ResidTags = IM.IntMap Int

emptyResidTags :: ResidTags
emptyResidTags = IM.empty

oneResidTag :: Tag -> ResidTags
oneResidTag (TG i _) = IM.singleton (unFin i) 1

plusResidTags :: ResidTags -> ResidTags -> ResidTags
plusResidTags = IM.unionWith (+)

plusResidTagss :: [ResidTags] -> ResidTags
plusResidTagss = IM.unionsWith (+)

-- FIXME:
--
--   let f = \x -> e
--   in <f>
--
-- Splits to:
--
--    let f = \x -> e
--    in <\x -> e>
--
-- For some reason that (\x -> e) actualy has the same tag as <f> (see Accumulator without rollback),
-- and disaster ensues...

--
-- == Gathering entry information for the splitter ==
--

type ContextId = Unique

data Entered = Once ContextId -- ^ If a binding is Entered twice from the same context it's really a single Entrance
             | Many           -- ^ A result of anything other than Once (or None, represented implicitly) is uninteresting for optimisation purposes
             deriving (Eq, Show)

instance Outputable Entered where
    ppr = text . show

isOnce :: Entered -> Bool
isOnce (Once _) = True
isOnce _        = False

plusEntered :: Entered -> Entered -> Entered
plusEntered (Once id1) (Once id2)
  | id1 == id2 = Once id1
  | otherwise  = Many
plusEntered _ _ = Many


type EnteredEnv = VarEnv Entered

mkEnteredEnv :: Entered -> FreeVars -> EnteredEnv
mkEnteredEnv ent = mapVarEnv (const ent)


--
-- == The splitter ==

-- Note [Phantom variables and bindings introduced by scrutinisation]
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
--
-- If we never introduced bindings from scrutinisation, the world of phantom bindings would be relatively
-- simple. In such a world, we would have this property:
--
--   The free variables of h-functions generated while supercompiling some term would never have
--   more free variables than the term being supercompiled
--
-- Unfortunately, this is not true in the real world. What can happen is this. We supercompile:
--  h1 x = case x of True -> e1; False -> e2
--
-- Which leads to the two recursively-supercompiled components:
--  h2 = let <x = True> in e1
--  h3 = let <x = False> in e2
--
-- Note that x was not static (free) in h1, but it is static (free) in h2. Thus, h-functions generated
-- during supercompilation (h2, h3) have more free variables than the term from which they were generated (h1).
--
--
-- Note [When to bind captured floats]
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
--
-- Ordinarily, we only need to check to see if we residualise some floating h-functions when we produce
-- a residual let-binding.  This is because in the normal course of things any binding that was originally
-- introduced as a lambda/alt-binding will never be made into a free variable of a final h-function.  However,
-- there are two situations which break this invariant:
--  1. We might choose to create a LetBound heap binding when driving the branches of a residual case expression
--     that scrutinises a free variable. This changes a LambdaBound thing into a LetBound one, so we need to be
--     careful to residualise the resulting h-function under that lambda-binder.
--
--     In fact, we used to do this but don't any more - see Note [Phantom variables and bindings introduced by scrutinisation]
--  2. More relevantly, we might implement an optimisation that prevents h-functions from being lambda-abstracted
--     over anything lambda-bound above a let-binding that we can see will trap the h-function under a let. For example,
--     when driving:
--
--       \x -> let f = \y -> ...
--             in D[<x |-> \lambda{}, f |-> l{\y -> ...} | ... f ... x ...>]
--
--     There is no point lambda-abstracting over x because we're going to have to drop the h-function under the f
--     binding anyway. To implement this we might drive with (x |-> l{}) instead, but once again this converts a
--     lambda-binding to a let-binding.
--
-- For this reason, we are careful to use bindCapturedFloats even when driving the arms of case expressions/bodies of lambdas.
--
--
-- Note [Bind captured floats fixed point]
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
-- Because bound h-functions (e.g. h2 or h3) may be referred to by other h-functions (e.g. h1) which do not
-- refer to any of the free variables of the h-functions we are about to bind, we have a fixed point in bindCapturedFloats.
-- This fixed point ensures we bind those h-functions that have as free variables any h-functions we are about to bind.


{-# INLINE split #-}
split :: MonadStatics m
      => State
      -> (State -> m (Deeds, Out FVedTerm))
      -> m (ResidTags, Deeds, Out FVedTerm)
split (deeds, Heap h ids, k, qa) opt
  = generaliseSplit opt ctxt_ids0 (IS.empty, emptyVarSet) deeds (Heap h ids, nameStack k, \ids -> (Just qa, splitQA ctxt_ids1 ids (annedTag qa) (annee qa)))
  where (ctxt_ids0, ctxt_ids1) = splitUniqSupply splitterUniqSupply

{-# INLINE instanceSplit #-}
instanceSplit :: MonadStatics m
              => (Deeds, Heap, Stack, Out FVedTerm)
              -> (State -> m (Deeds, Out FVedTerm))
              -> m (ResidTags, Deeds, Out FVedTerm)
instanceSplit (deeds, heap, k, focus) opt = generaliseSplit opt splitterUniqSupply (IS.empty, emptyVarSet) deeds (heap, nameStack k, \_ -> (Nothing, noneBracketed' IM.empty focus)) -- FIXME: residualised tags? FIXME: scrutinee identity?
-- TODO: arguably I should try to get a QA for the thing in the focus. This will help in cases like where we MSG together:
--  < H | v | >
-- and:
--  < H, H' | v | update f >
-- Since ideally instance splitting the second state should allow us to drive H' with the value binding f |-> v. A similar argument applies to questions in focus.

nameStack :: Stack -> NamedStack
nameStack = snd . trainCarMapAccumL (\i kf -> (i + 1, (i, kf))) 0

{-# INLINE generalise #-}
generalise :: MonadStatics m
           => Generaliser
           -> State
           -> Maybe ((State -> m (Deeds, Out FVedTerm)) -> m (ResidTags, Deeds, Out FVedTerm))
generalise gen (deeds, Heap h ids, k, qa) = do
    let named_k = nameStack k
    
    (gen_kfs, gen_xs') <- case sPLIT_GENERALISATION_TYPE of
        NoGeneralisation -> Nothing
        AllEligible -> guard (not (IS.null gen_kfs) || not (isEmptyVarSet gen_xs'')) >> return (gen_kfs, gen_xs'')
          where gen_kfs = IS.fromList [i   | (i, kf) <- trainCars named_k, generaliseStackFrame gen kf]
                gen_xs'' = mkVarSet [x'' | (x'', hb) <- M.toList h, generaliseHeapBinding gen x'' hb, ASSERT2(not (howBound hb == LambdaBound && isNothing (heapBindingTerm hb)), ppr (x'', hb, heapBindingTag hb)) True]
        StackFirst -> (guard (not (IS.null gen_kfs)) >> return (gen_kfs, emptyVarSet)) `mplus`
                      (guard (not (isEmptyVarSet gen_xs''))  >> return (IS.empty, gen_xs''))
          where gen_kfs = IS.fromList [i   | (i, kf) <- trainCars named_k, generaliseStackFrame gen kf]
                gen_xs'' = mkVarSet [x'' | (x'', hb) <- M.toList h, generaliseHeapBinding gen x'' hb, ASSERT2(not (howBound hb == LambdaBound && isNothing (heapBindingTerm hb)), ppr (x'', hb, heapBindingTag hb)) True]
        DependencyOrder want_first -> listToMaybe ((if want_first then id else reverse) possibilities)
          where -- We consider possibilities starting from the root of the term -- i.e. the bottom of the stack.
                -- This is motivated by how the interaction with subgraph generalisation for TreeFlip/TreeSum.
                -- FIXME: explain in more detail if this turns out to be sane.
                possibilities = findGeneralisable False emptyVarSet (reverse (trainCars named_k)) h
                
                findGeneralisable done_qa pending_xs' unreached_kfs unreached_hbs
                   | done_qa && null pending_kfs && M.null pending_hbs
                   = []
                   | otherwise
                   = [(gen_kf_is, gen_xs'') | not (IS.null gen_kf_is) || not (isEmptyVarSet gen_xs'')] ++
                     findGeneralisable done_qa' reached_xs' unreached_kfs' unreached_hbs'
                  where
                    (done_qa', extra_pending_xs') = if done_qa || not (null unreached_kfs) then (done_qa, emptyVarSet) else (True, annedFreeVars qa)
                    (pending_kfs, unreached_kfs') = splitAt 1 unreached_kfs
                    (pending_hbs, unreached_hbs') = M.partitionWithKey (\x' _hb -> x' `elemVarSet` (pending_xs' `unionVarSet` extra_pending_xs')) unreached_hbs
                    
                    gen_kf_is = IS.fromList [i  | (i, kf) <- pending_kfs, generaliseStackFrame gen kf]
                    gen_xs'' = mkVarSet [x'' | (x'', hb) <- M.toList pending_hbs, generaliseHeapBinding gen x'' hb, ASSERT2(not (howBound hb == LambdaBound && isNothing (heapBindingTerm hb)), ppr (x'', hb, heapBindingTag hb)) True]
                    
                    reached_xs' = M.foldrWithKey (\_x' hb fvs -> heapBindingFreeVars hb `unionVarSet` fvs)
                                                 (unionVarSets (map (stackFrameFreeVars . tagee . snd) pending_kfs))
                                                 pending_hbs
    
    -- If we can find some fraction of the stack or heap to drop that looks like it will be admissable, just residualise those parts and continue
    pprTrace "generalise: (gen_kfs, gen_xs')" (ppr (gen_kfs, gen_xs')) $ return ()
    
    let (ctxt_id, ctxt_ids) = takeUniqFromSupply splitterUniqSupply
    return $ \opt -> generaliseSplit opt ctxt_ids (gen_kfs, gen_xs') deeds (Heap h ids, named_k, \ids -> (Just qa, oneBracketed' (qaType qa) (Once ctxt_id, (emptyDeeds, Heap M.empty ids, Loco False, annedQAToInAnnedTerm ids qa))))

{-# INLINE generaliseSplit #-}
generaliseSplit :: MonadStatics m
                => (State -> m (Deeds, Out FVedTerm))
                -> UniqSupply
                -> (IS.IntSet, Out VarSet)
                -> Deeds
                -> (Heap, NamedStack, InScopeSet -> (Maybe (Anned QA), Bracketed (Entered, UnnormalisedState)))
                -> m (ResidTags, Deeds, Out FVedTerm)
generaliseSplit opt ctxt_ids split_from deeds (heap, named_k, focus) = optimiseSplit opt deeds'' bracketeds_heap bracketed_focus
  where -- After we complete cheapification, the in-scope-set will not change at all, so we can poke it into the focus
        (deeds', heap'@(Heap _ ids')) = cheapifyHeap deeds heap
        (deeds'', bracketeds_heap, bracketed_focus) = splitt ctxt_ids split_from deeds' (heap', named_k, focus ids')

-- Discard dead bindings:
--  let x = ...
--  in 1
-- ==>
--  1
--
-- But include transitively needed ones:
--  let w = ...
--      x = ...
--      y = ... x ...
--      z = ... y ...
--  in z
-- ==>
--  let z = let x = ...
--              y = ... x ...
--          in ... y ...
--  in z
--
-- Inline values and linear things into residual bindings:
--  let x = ... y ...
--      y = ...
--  in \_ -> ... x ...
-- ===>
--  let x = let y = ...
--          in ... y ...
--  in \_ -> ... x ...
--
-- Inline values into residual non-linear things:
--  let x = (y:ys)
--  in \_ -> ... x ...
-- ==>
--  \_ -> let x = (y:ys)
--        in ... x ...
--
-- Do NOT inline linear things into non-linear things:
--  let x = (y:ys)
--      y = ...
--  in \_ -> ... x ...
-- =/=>
-- \_ -> let x = let y = ...
--               in (y:ys)
--       in ... x ...
-- ===>
--  let y = ...
--  in \_ -> let x = (y:ys)
--           in ... x ...
--
-- Inline things that are (apparently) used non-linearly times into linear things:
--  let w = ...
--      x = ... w ...
--      y = ... w ...
--      z = (x, y)
--  in Just z
-- ===>
--  let z = let w = ...
--              x = ... w ...
--              y = ... w ...
--          in (x, y)
--  in Just z
--
-- Treat non-linearity due only to |case| branches as linearity:
--  let x = ...
--  in case unk of C -> ... x ...; D -> ... x ...
-- ===>
--  case unk of C -> let x = ... in ... x ...
--              D -> let x = ... in ... x ...
--
-- Let-float things to trivialise them:
--  let x = let y = ... in (y:xs)
--  in \_ -> ... x ...
-- ===>
--  let y = ....
--  \_ -> let x = (y:xs) in ... x ...
--
-- Note [EC binds something we need to refer to above]
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
--   let z = f x
--       y = unk + z
--       x = case y of _ -> 2
--   in x + 2
--
-- After splitting, we might want to drive this child term:
--   let x = 2
--   in x + 2
-- 
-- That's fine, but how are we going to get a reference to the "x" when residualising the y binding above?
--  let z = f x
--      y = unk + z
--  in case y of _ -> h0
--
-- Lacking extra language features, our only option is to under-specialise the floats by inlining less
-- evaluation context.
data Shell = Shell {
    shellExtraTags :: IM.IntMap Int,
    shellExtraFvs  :: FreeVars,                      -- ^ Maximum free variables added by the residual wrapped around the holes
    shellWrapper   :: [Out FVedTerm] -> Out FVedTerm -- ^ Wrap the contents of the holes
  }


data Hole a = Hole {
    holeBvs    :: [Var], -- ^ Maximum bound variables added at each hole by the residual wrapped around the holes, most tightly binding last
    holeFiller :: a      -- ^ Hole-fillers themselves. Usually State
  }

instance Functor Hole where fmap = fmapDefault
instance Foldable Hole where foldMap = foldMapDefault

instance Traversable Hole where
    traverse f h = Hole (holeBvs h) <$> f (holeFiller h)

instance Accumulatable Hole where
    mapAccumTM f acc h = liftM (\(acc', filler') -> (acc', h { holeFiller = filler' })) $ f acc (holeFiller h)


data TailishHole a = TailishHole {
    tailishIsTailHole :: Bool,
    tailishHole :: Hole a
  }

instance Functor TailishHole where fmap = fmapDefault
instance Foldable TailishHole where foldMap = foldMapDefault

instance Traversable TailishHole where
    traverse f h = TailishHole (tailishIsTailHole h) <$> traverse f (tailishHole h)

instance Accumulatable TailishHole where
    mapAccumTM f acc h = liftM (\(acc', hole') -> (acc', h { tailishHole = hole' })) $ mapAccumTM f acc (tailishHole h)


data Bracketed a = TailsKnown Type (Type -> Shell) [TailishHole a]
                 | TailsUnknown             Shell  [Hole a]

instance Functor Bracketed where fmap = fmapDefault
instance Foldable Bracketed where foldMap = foldMapDefault

instance Traversable Bracketed where
    traverse f (TailsKnown ty mk_shell holes) = TailsKnown ty mk_shell <$> traverse (traverse f) holes
    traverse f (TailsUnknown  shell    holes) = TailsUnknown shell     <$> traverse (traverse f) holes

instance Accumulatable Bracketed where
    mapAccumTM f acc (TailsKnown ty mk_shell holes) = liftM (second (TailsKnown ty mk_shell)) $ mapAccumTM (mapAccumTM f) acc holes
    mapAccumTM f acc (TailsUnknown  shell    holes) = liftM (second (TailsUnknown shell))     $ mapAccumTM (mapAccumTM f) acc holes

noneBracketed :: Tag -> Out FVedTerm -> Bracketed a
noneBracketed = noneBracketed' . oneResidTag

noneBracketed' :: IM.IntMap Int -> Out FVedTerm -> Bracketed a
noneBracketed' tgs a = TailsUnknown (Shell { shellExtraTags = tgs, shellExtraFvs = freeVars a, shellWrapper = \[] -> a }) []

-- NB: I could use normalise here to make my life easier if transitiveInline didn't treat Bracketed heaps specially
--
-- NB: it is VERY IMPORTANT that you use oneBracketed' instead in contexts where you might want to use the tails of the bracketed.
-- In particular, if you use oneBracketed to prepare the branches of a case expression then map-map fusion won't work!
oneBracketed :: UniqSupply -> Type -> (Entered, (Heap, Stack, In AnnedTerm)) -> Bracketed (Entered, UnnormalisedState)
oneBracketed ctxt_ids ty (ent, (Heap h ids, k, in_e))
  | eAGER_SPLIT_VALUES
  , Just (cast_by, mb_update) <- isTrivialStack_maybe k -- NB: this might find a cast even when we have an answer in the context since the state is unnormalised
  , Just anned_a0 <- termToCastAnswer ids in_e -- NB: I could push extra heap into the bracketed_a1 using the mb_update if it is Just, but I don't think I usually need to
  , let anned_a1 = castByAnswer ids cast_by (annedToTagged anned_a0)
        bracketed_a1 = fmap (\(ent', (deeds, Heap h' ids', k', in_e')) -> (if isOnce ent then ent' else Many, (deeds, Heap (h' `M.union` h) ids', k', in_e'))) $ -- Push heap of positive information/new lambda-bounds down + fix hole Entereds
                       modifyShell (\shell -> shell { shellExtraFvs = shellExtraFvs shell `minusVarSet` fst (pureHeapVars h) LambdaBound }) $                    -- Fix bracket FVs by removing anything lambda-bound above
                       splitCoerced (splitAnswer ctxt_ids ids) anned_a1
  = case mb_update of
      Nothing                          -> bracketed_a1
      Just (Tagged tg_x' x', cast_by') -> zipBracketeds (TailsUnknown shell [Hole { holeBvs = [x'], holeFiller = bracketed_a1 }])
        where shell = case cast_by' of
                CastBy co co_tg -> Shell { shellExtraTags = oneResidTag tg_x' `plusResidTags` oneResidTag co_tg, shellExtraFvs = tyCoVarsOfCo co, shellWrapper = \[e'] -> letRec [(x', e')] (var x' `cast` co) }
                Uncast          -> Shell { shellExtraTags = oneResidTag tg_x',                                   shellExtraFvs = emptyVarSet,     shellWrapper = \[e'] -> letRec [(x', e')] (var x') }
  | otherwise
  = oneBracketed' ty (ent, (emptyDeeds, Heap h ids, k, in_e))

oneBracketed' :: Type -> a -> Bracketed a
oneBracketed' ty x = TailsKnown ty (\_ -> Shell { shellExtraTags = IM.empty, shellExtraFvs = emptyVarSet, shellWrapper = \[e] -> e }) [TailishHole True (Hole [] x)]

zipBracketeds :: Bracketed (Bracketed a)
              -> Bracketed a
zipBracketeds (TailsUnknown bshell bholes) = TailsUnknown (Shell shell_tags shell_fvs (\es -> shell_wrapper es [])) holes
  where (shell_tags, shell_fvs, shell_wrapper, holes) = foldr go (shellExtraTags bshell, shellExtraFvs bshell, \[] rev_es' -> shellWrapper bshell (reverse rev_es'), []) bholes
        go (Hole bvs bracketed) (shell_extra_tags, shell_extra_fvs, shell_wrapper, holes)
          = (plusResidTags shell_extra_tags (bracketedExtraTags rbracketed),
             shell_extra_fvs `unionVarSet` nonRecBindersFreeVars bvs (bracketedExtraFvs rbracketed),
             \es rev_es' -> case splitBy (bracketedHoles rbracketed) es of
                              (es_here, Right es_later) -> shell_wrapper es_later (shellWrapper (bracketedShell rbracketed) es_here:rev_es'),
             bracketedHoles rbracketed ++ holes)
          where rbracketed = rigidizeBracketed bracketed
zipBracketeds (TailsKnown bty mk_bshell bholes) = case ei_holes of
    Left holes  -> TailsUnknown          (Shell (mk_shell_tags bty) (mk_shell_fvs bty) (\es -> mk_shell_wrapper bty es [])) holes
    Right holes -> TailsKnown bty (\ty -> Shell (mk_shell_tags ty)  (mk_shell_fvs ty)  (\es -> mk_shell_wrapper ty  es [])) holes
  where (mk_shell_tags, mk_shell_fvs, mk_shell_wrapper, ei_holes) = foldr go (\ty -> shellExtraTags (mk_bshell ty),
                                                                              \ty -> shellExtraFvs (mk_bshell ty),
                                                                              \ty [] rev_es' -> shellWrapper (mk_bshell ty) (reverse rev_es'), Right []) bholes
        go (TailishHole is_tail (Hole bvs bracketed)) (shell_extra_tags, shell_extra_fvs, shell_wrapper, ei_holes) = case bracketed of
          TailsKnown _ty mk_shell holes'
            | is_tail, Right holes <- ei_holes
            -> (\ty -> plusResidTags (shell_extra_tags ty) (shellExtraTags (mk_shell ty)),
                \ty -> shell_extra_fvs ty `unionVarSet` nonRecBindersFreeVars bvs (shellExtraFvs (mk_shell ty)),
                \ty es rev_es' -> case splitBy holes' es of
                                    (es_here, Right es_later) -> shell_wrapper ty es_later (shellWrapper (mk_shell ty) es_here:rev_es'),
                Right (holes' ++ holes))
          _ -> (\ty -> plusResidTags (shell_extra_tags ty) (bracketedExtraTags rbracketed),
                \ty -> shell_extra_fvs ty `unionVarSet` nonRecBindersFreeVars bvs (bracketedExtraFvs rbracketed),
                \ty es rev_es' -> case splitBy (bracketedHoles rbracketed) es of
                                    (es_here, Right es_later) -> shell_wrapper ty es_later (shellWrapper (bracketedShell rbracketed) es_here:rev_es'),
                case ei_holes of Left holes -> Left (bracketedHoles rbracketed ++ holes)
                                 Right holes | is_tail   -> Left (bracketedHoles rbracketed ++ map tailishHole holes)
                                             | otherwise -> Right (map (TailishHole False) (bracketedHoles rbracketed) ++ holes))
            where rbracketed = rigidizeBracketed bracketed

modifyShell :: (Shell -> Shell) -> Bracketed a -> Bracketed a
modifyShell f (TailsKnown ty mk_shell holes) = TailsKnown ty (f . mk_shell) holes
modifyShell f (TailsUnknown  shell    holes) = TailsUnknown  (f shell)      holes

modifyTails_ :: (Type -> Type) -> ([a] -> [a]) -> Bracketed a -> Maybe (Bracketed a)
modifyTails_ mk_ty f = fmap snd . modifyTails mk_ty (\as -> ((), f as))

modifyTails :: forall a b. (Type -> Type) -> ([a] -> (b, [a])) -> Bracketed a -> Maybe (b, Bracketed a)
modifyTails _     _ (TailsUnknown _ _)             = Nothing
modifyTails mk_ty f (TailsKnown ty mk_shell holes) = Just (b, TailsKnown (mk_ty ty) mk_shell holes')
  where (b, holes') = traverseSome (tailishIsTailHole :: TailishHole a -> Bool) (second unComp . traverseAll f . Comp) (holes :: [TailishHole a])


data RBracketed a = RBracketed { bracketedShell :: Shell, bracketedHoles :: [Hole a] }

instance Functor RBracketed where fmap = fmapDefault
instance Foldable RBracketed where foldMap = foldMapDefault

instance Traversable RBracketed where traverse f (RBracketed shell holes) = RBracketed shell <$> traverse (traverse f) holes

bracketedExtraTags :: RBracketed a -> IM.IntMap Int
bracketedExtraTags = shellExtraTags . bracketedShell

bracketedExtraFvs :: RBracketed a -> FreeVars
bracketedExtraFvs = shellExtraFvs . bracketedShell

bracketedFreeVars :: (a -> FreeVars) -> RBracketed a -> FreeVars
bracketedFreeVars fvs rbracketed = bracketedExtraFvs rbracketed `unionVarSet` unionVarSets [nonRecBindersFreeVars (holeBvs hole) (fvs (holeFiller hole)) | hole <- bracketedHoles rbracketed]

rigidizeBracketed :: Bracketed a -> RBracketed a
rigidizeBracketed (TailsKnown ty mk_shell holes) = RBracketed (mk_shell ty) (map tailishHole holes)
rigidizeBracketed (TailsUnknown  shell    holes) = RBracketed shell         holes


optimiseMany :: Monad m
             => ((Deeds, a) -> m (ResidTags, Deeds, b))
             -> (Deeds, [a])
             -> m (ResidTags, Deeds, [b])
optimiseMany opt (deeds, xs) = liftM fixup $ mapAccumLM opt' deeds xs
  where opt' x y = liftM (\(resid_tags, deeds, b) -> (deeds, (resid_tags, b))) $ opt (x, y)
        fixup (deeds, resid_tagss_bs) = (plusResidTagss resid_tagss, deeds, bs)
          where (resid_tagss, bs) = unzip resid_tagss_bs

optimiseBracketed :: MonadStatics m
                  => (State -> m (Deeds, Out FVedTerm))
                  -> (Deeds, RBracketed State)
                  -> m (ResidTags, Deeds, Out FVedTerm)
optimiseBracketed opt (deeds, rbracketed) = liftM (\(resid_tags, deeds, es) -> (shellExtraTags shell `plusResidTags` resid_tags, deeds, shellWrapper shell es)) $ optimiseMany optimise_one (deeds, bracketedHoles rbracketed)
  where shell = bracketedShell rbracketed
        optimise_one (deeds, (Hole extra_bvs (s_deeds, s_heap, s_k, s_e))) = liftM (\(xes, (deeds, e)) -> (IM.empty, deeds, bindManyMixedLiftedness fvedTermFreeVars xes e)) $ bindCapturedFloats (mkVarSet extra_bvs) $ opt (deeds `plusDeeds` s_deeds, s_heap, s_k, s_e)
        -- Because h-functions might potentially refer to the lambda/case-alt bound variables around this hole,
        -- we use bindCapturedFloats to residualise such bindings within exactly this context.
        -- See Note [When to bind captured floats]


-- TODO: when driving a residual binding:
--   let x = D[e]
--   in ..
--
-- Arjan Boeijink suggested driving the following instead of D[e]:
--   D[< | e | update x>]
--
-- This can help propagate more positive information, e.g. if e contains an occurrence of x itself
--
-- I'm not doing this right now because I'm wary about the termination issues. We should also be careful that we
-- don't create loops as a result...

data BracketedStuff a = BracketedStuff (RBracketed a) (M.Map (Out Var) (RBracketed a))

instance Functor BracketedStuff where fmap = fmapDefault
instance Foldable BracketedStuff where foldMap = foldMapDefault

instance Traversable BracketedStuff where
    traverse f (BracketedStuff a b) = BracketedStuff <$> traverse f a <*> traverse (traverse f) b

optimiseSplit :: MonadStatics m
              => (State -> m (Deeds, Out FVedTerm))
              -> Deeds
              -> M.Map (Out Var) (RBracketed State)
              -> RBracketed State
              -> m (ResidTags, Deeds, Out FVedTerm)
optimiseSplit opt deeds bracketeds_heap bracketed_focus = {-# SCC "optimiseSplit'" #-}do
    -- 0) The "process tree" splits at this point. We can choose to distribute the deeds between the children in a number of ways
    let (deeds_initial, BracketedStuff bracketed_deeded_focus bracketeds_deeded_heap) = flip traverseAll (BracketedStuff bracketed_focus bracketeds_heap) $
          \states -> let (deeds_initial:deedss) = splitDeeds deeds (1:map stateSize states)
                     in (deeds_initial, zipWithEqual "optimiseSplit" addStateDeeds deedss states)
    
    MASSERT2(noChange (sumMapMonoid (sumMapMonoid releaseStateDeed) bracketeds_heap        `plusDeeds` sumMapMonoid releaseStateDeed bracketed_focus        `plusDeeds` deeds)
                      (sumMapMonoid (sumMapMonoid releaseStateDeed) bracketeds_deeded_heap `plusDeeds` sumMapMonoid releaseStateDeed bracketed_deeded_focus `plusDeeds` deeds_initial),
             ppr deeds)
    
    (hes, (resid_tags, deeds, xes, e_focus)) <- bindCapturedFloats (dataSetToVarSet (M.keysSet bracketeds_heap)) $ do
        -- 1) Recursively drive the focus itself
        (fvs_focus_hs, (resid_tags0, leftover_deeds, e_focus)) <- monitorFVs $ optimiseBracketed opt (deeds_initial, bracketed_deeded_focus)
        
        -- 2) We now need to think about how we are going to residualise the letrec. In fact, we need to loop adding
        -- stuff to the letrec because it might be the case that:
        --  * One of the hes from above refers to some heap binding that is not referred to by the let body
        --  * So after we do withStatics above we need to drive some element of the bracketeds_heap
        --  * And after driving that we find in our new hes a new h function referring to a new free variable
        --    that refers to some binding that is as yet unbound...
        let resid_fvs = fvs_focus_hs `unionVarSet` fvedTermFreeVars e_focus
        (resid_tags1, leftover_deeds, bracketeds_deeded_heap, _fvs, xes) <- optimiseLetBinds opt leftover_deeds bracketeds_deeded_heap resid_fvs
    
        return (resid_tags0 `plusResidTags` resid_tags1, sumMapMonoid (sumMapMonoid releaseStateDeed) bracketeds_deeded_heap `plusDeeds` leftover_deeds, xes, e_focus)
    
    -- 3) Combine the residualised let bindings with the let body
    return (resid_tags, deeds, bindManyMixedLiftedness fvedTermFreeVars (xes ++ hes) e_focus)


-- We only want to drive (and residualise) as much as we actually refer to. This loop does this: it starts
-- by residualising the free variables of the focus residualisation (or whatever is in the let body),
-- and then transitively inlines any bindings whose corresponding binders become free.
optimiseLetBinds :: MonadStatics m
                 => (State -> m (Deeds, Out FVedTerm))
                 -> Deeds
                 -> M.Map (Out Var) (RBracketed State)
                 -> FreeVars
                 -> m (ResidTags, Deeds, M.Map (Out Var) (RBracketed State), FreeVars, Out [(Var, FVedTerm)])
optimiseLetBinds opt leftover_deeds bracketeds_heap fvs' = -- traceRender ("optimiseLetBinds", M.keysSet bracketeds_heap, fvs') $
                                                           go IM.empty leftover_deeds bracketeds_heap [] fvs'
  where
    go resid_tags leftover_deeds bracketeds_deeded_heap_not_resid xes_resid resid_fvs
      | M.null h_resid = return (resid_tags, leftover_deeds, bracketeds_deeded_heap_not_resid, resid_fvs, xes_resid)
      | otherwise = {- traceRender ("optimiseSplit", xs_resid') $ -} do
        -- Recursively drive the new residuals arising from the need to bind the resid_fvs
        (fvs_es_hs, (extra_resid_tags, leftover_deeds, es_resid')) <- monitorFVs $ optimiseMany (optimiseBracketed opt) (leftover_deeds, bracks_resid)
        -- Recurse, because we might now need to residualise and drive even more stuff (as we have added some more FVs)
        let resid_fvs_delta = fvs_es_hs `unionVarSet` unionVarSets (map fvedTermFreeVars es_resid')
        go (resid_tags `plusResidTags` extra_resid_tags)
           leftover_deeds
           bracketeds_deeded_heap_not_resid'
           (xes_resid ++ zip xs_resid' es_resid')
           (resid_fvs `unionVarSet` resid_fvs_delta)
      where
        -- When assembling the final list of things to drive, ensure that we exclude already-driven things
        (h_resid, bracketeds_deeded_heap_not_resid') = M.partitionWithKey (\x _br -> x `elemVarSet` resid_fvs) bracketeds_deeded_heap_not_resid
        (xs_resid', bracks_resid) = unzip $ M.toList h_resid

-- TODO: I could use the improved entered info that comes from the final FVs to adjust the split and float more stuff inwards..

type NamedStack = Train (Int, Tagged StackFrame) Generalised

splitt :: UniqSupply
       -> (IS.IntSet, Out VarSet)
       -> Deeds
       -> (Heap, NamedStack, (Maybe (Anned QA), Bracketed (Entered, UnnormalisedState))) -- ^ The thing to split, and the Deeds we have available to do it
       -> (Deeds,                              -- The Deeds still available after splitting
           M.Map (Out Var) (RBracketed State), -- The residual "let" bindings
           RBracketed State)                   -- The residual "let" body
splitt ctxt_ids (gen_kfs, gen_xs) deeds (Heap h ids, named_k, (mb_anned_qa, bracketed_qa))
    = {-# SCC "splitt'" #-} snd $ split_step split_fp
      -- Once we have the correct fixed point, go back and grab the associated information computed in the process
      -- of obtaining the fixed point. That is what we are interested in, not the fixed point itselF!
      -- TODO: eliminate redundant recomputation here?
  where    
    (ctxt_ids0, ctxt_ids1) = splitUniqSupply ctxt_ids

    -- We compute the correct way to split as a least fixed point, slowly building up a set of variables
    -- (bound by heap bindings and update frames) that it is safe *not* to residualise.
    --
    -- Note that as an optimisation, optimiseSplit will only actually creates those residual bindings if the
    -- corresponding variables are free *after driving*. Of course, we have no way of knowing which bindings
    -- will get this treatment here, so just treat resid_xs as being exactly the set of residualised stuff.
    split_fp = lfpFrom (\(xs1, ys1) (xs2, ys2) -> (xs1 `unionVarSet` xs2, ys1 `unionVarSet` ys2)) (emptyVarSet, emptyVarSet) (fst . split_step)
    
    -- Simultaneously computes the next fixed-point step and some artifacts computed along the way,
    -- which happen to correspond to exactly what I need to return from splitt.
    split_step (safe_not_resid_xs, deeds_resid_xs) = -- let pPrintBracketedState = map pPrintFullState . fillers in traceRender ("split_step", (not_resid_xs, bound_xs S.\\ not_resid_xs), pureHeapBoundVars h_not_residualised, pureHeapBoundVars h_residualised, M.map pPrintBracketedState bracketeds_heap', pPrintBracketedState bracketed_focus') $
                                                     ((safe_not_resid_xs', deeds_resid_xs'), (deeds4, bracketeds_heap', bracketed_focus'))
      where
        -- 0) Compute the set of variables that I can *actually* get away without residualising, once deeds are accounted for
        -- See Note [Deeds and splitting] for further details on this.
        not_resid_xs = safe_not_resid_xs `minusVarSet` deeds_resid_xs
        
        -- 1) Build a candidate splitting for the Stack and QA components
        -- When creating the candidate stack split, we ensure that we create a residual binding
        -- for any variable in the resid_xs set, as we're not going to inline it to continue.
        --
        -- NB: do NOT normalise at this stage because in transitiveInline we assume that State heaps are droppable!
        scruts = case fmap annee mb_anned_qa of Just (Question x') -> [x']; _ -> []
        (deeds1a, bracketeds_updated, bracketed_focus)
          = pushStack ctxt_ids0 ids deeds scruts (fmapCars (\(i, kf) -> (need_not_resid_kf i kf, kf)) named_k) bracketed_qa
            
        need_not_resid_kf i kf
          | i `IS.member` gen_kfs
          = False
          | Update x' <- tagee kf -- We infer the stack frames we're not residualising based on the *variables* we're not residualising
          = x' `elemVarSet` not_resid_xs
          | otherwise
          = True
        
        -- 2) Build a splitting for those elements of the heap we propose to residualise not in not_resid_xs
        -- TODO: I should residualise those Unfoldings whose free variables have become interesting due to intervening scrutinisation
        (h_not_residualised, h_residualised) = M.partitionWithKey (\x' _ -> x' `elemVarSet` not_resid_xs) h
        bracketeds_nonupdated0 = M.mapMaybeWithKey (\x' hb -> do { guard (howBound hb == InternallyBound); return $ case heapBindingTerm hb of Nothing -> (error "Unimplemented: no tag for undefined", undefined "FIXME FIXME" (noneBracketed (error "No tag for undefined") (fvedTerm (Var (undefined "tcLookupId" undefinedName))))); Just in_e@(_, e) -> (annedTag e, oneBracketed ctxt_ids1 (idType x') (Once (idUnique x'), (Heap M.empty ids, Loco False, in_e))) }) h_residualised
        -- An idea from Arjan, which is sort of the dual of positive information propagation:
        -- TODO: this is too dangerous presently: we often end up adding an Update at the end just after we generalised it away, building ourselves a nice little loop :(
        -- I have tried to work around this by only introducing Update frames for those things that don't presently have one... but that also doesn't work because if we start
        -- with (let x = v in x) then we reduce to (let x = v in \underbar{x}) and then split to (let x = v in x)
        --(deeds1, bracketeds_nonupdated) = M.mapAccumWithKey (\deeds x' (update_tg, brack) -> modifyTails (\states -> case claimDeeds deeds (length states) of Nothing -> (deeds, states); Just deeds -> (deeds, map (\(entered, (deeds, heap, k, in_e)) -> (entered, (deeds, heap, k ++ [Tagged update_tg (Update x')], in_e))) states)) brack `orElse` (deeds, brack)) deeds1a bracketeds_nonupdated0
        (deeds1, bracketeds_nonupdated) = (deeds1a, M.map snd bracketeds_nonupdated0)
        -- For every heap binding we ever need to deal with, contains a version of that heap binding as a concrete Bracketed thing
        bracketeds_heap = bracketeds_updated `M.union` bracketeds_nonupdated
        
        -- 3) Inline as much of the Heap as possible into the candidate splitting
        
        -- 3a) Release deeds
        -- In order to make the Deeds-based stuff less conservative, my first action here is to release our claims to those deeds
        -- which we do *not* intend to create a residual let binding for here and now. This will let us always inline a heap-bound
        -- thing into *at least one* context (unless it really is referred to by the residual code).
        --
        -- The equivalent process is done for the stack in splitStack itself: we just subtract 1 from the number of deeds we need to
        -- claim when duplicating a stack frame.
        deeds2 = releasePureHeapDeeds deeds1 h_not_residualised
        
        -- 3b) Work out which part of the heap is admissable for inlining
        -- * We are allowed to inline concrete things which are duplicatable or are not residualised right here and now
        -- * Non-concrete stuff should be inlined if and only if it is not explicitly residualised by the caller. The motivation that
        --   if we generalise away a term, we want to generalise away the staticness as well. Furthermore, it is clear that if we are
        --   just generalising away staticness itself we certainly should not push the corresponding non-concrete binding down.
        -- * We take this opportunity to mark all residualised things as static (being careful to not override actual definitions in h_cheap).
        --   It important that we do not mark residualised things as phantoms just because they are in bracketeds_heap. If we did, it would mean
        --   that *concrete residualised stuff* is recorded as a phantom even if it was explicitly residualised in the initial iteration (since
        --   anything residualised in the first iteration is certainly in bracketeds_heap).
        -- * If we are inlining a value (into a non-linear context), we are careful to only inline an *indirection* to that value. That
        --   allows us to prevent duplicating the allocation of such values. NB: we still duplicate allocation of cheap non-values, but never mind...
        --
        -- Inlineable things are either:
        --  1) Heap bindings from the input (i.e from the heap and update frames) that have not been residualised for work duplication reasons
        --  2) Concrete values and cheap expressions from the input, in a form that is suitable for pushing down (i.e. values have been turned into indirections).
        --  3) Phantom versions of phantom input heap bindings (just copied verbatim).
        --  4) Phantom versions of concrete input heap bindings
        -- The range of this heap is lte that of bracketeds_heap. We explicitly EXCLUDE those bindings that we are residualising based on the generalisation heuristic.
        -- We prefer input heap bindings to everything else, and concrete values/cheap expressions to phantoms. For example, even if a value is residualised, we would
        -- like to push down *some* version of it, hence the h_cheap full of indirections. And even if a concrete term is residualised we'd like a phantom version of it.
        --
        -- Basically the idea of this heap is "stuff we want to make available to push down"
        h_updated_phantoms = M.fromDistinctAscList [(x', lambdaBound) | x' <- M.keys bracketeds_updated] -- TODO: move this into h_cheap_and_phantoms?
        h_inlineable = varSetToDataMap generalisedLambdaBound gen_xs `M.union` -- The exclusion just makes sure we don't inline explicitly generalised bindings (even phantom ones)
                       (h_not_residualised `M.union`                           -- Take any non-residualised bindings from the input heap/stack...
                        h_cheap_and_phantom `M.union`                          -- ...failing which, take concrete definitions for cheap heap bindings (even if they are also residualised) or phantom definitions for expensive ones...
                        h_updated_phantoms)                                    -- ...failing which, take phantoms for things bound by update frames (if supercompilation couldn't turn these into values, GHC is unlikely to get anything good from seeing defs)
        
        -- Generalising the final proposed floats may cause some bindings that we *thought* were going to be inlined to instead be
        -- residualised. We need to account for this in the Entered information (for work-duplication purposes), and in that we will
        -- also require any FVs of the new residualised things that are bound in the stack to residualise more frames.
        inlineHeapT :: Accumulatable t
                    => (Deeds -> a   -> (Deeds, EnteredEnv, b))
                    ->  Deeds -> t a -> (Deeds, EnteredEnv, t b)
        inlineHeapT f deeds b = (deeds', entered', b')
          where ((deeds', entered'), b') = mapAccumT (\(deeds, entered) s -> case f deeds s of (deeds, entered', s) -> ((deeds, plusVarEnv_C plusEntered entered entered'), s)) (deeds, emptyVarEnv) b

        -- Like inlineHeapT, but removes from the EnteredEnv any mention of the actual binder being analysed, so we push more stuff down
        -- NB: this would be (partially?) subsumed if we found a way to push an Update frame for such a thing into its Bracketed, since then it wouldn't even be a FV
        --
        -- This is required to prevent self-recursive heap bindings from being unconditionally residualised:
        --  let xs = x : xs
        --      z = head xs
        --  in Just z
        -- ==>
        --  let z = let xs = x : xs
        --          in head xs
        --  in Just z
        --
        -- Without this hack, xs is Once from both z and xs, so it is Many overall and can't be inlined.
        --
        -- More generally, we might have mutual recursion:
        --  let xs = x : ys
        --      ys = y : xs
        --      z = head xs + head ys
        --  in Just z
        -- ==>
        --  let z = let xs = x : xs
        --              ys = y : ys
        --          in head xs + head ys
        --  in Just z
        --
        -- To deal with the more general case, we have to identify strongly-connected-components in the
        -- graph of heap Bracketed things. This is well-motivated because SCCs must be either pushed
        -- or residualised as a group.
        --
        -- Note [Greatest fixed point]
        -- ~~~~~~~~~~~~~~~~~~~~~~~~~~~
        -- You are probably thinking that this is all an awful workaround for the fact that I'm not using a greatest fixed point. This is true.
        -- However, although GFP would work perfectly in the example above, it is difficult to get right in general because we don't know exactly how
        -- to grow the set of "always residualise" bindings/frames on each iteration. Consider:
        --
        --  let x = e1
        --      y = e2[x]
        --  in \z -> e3[y]
        --
        -- After optimistic GFP inlining we get:
        --
        --  \z -> let x = e1
        --            y = e2[x]
        --        in e3[y]
        --
        -- But now we've duplicated computation! We can see this because the post-inlining entered info for the non-value bindings x and y is
        -- both \infty. However, we can't on the next iteration just mark both x and y as "always residualise" because then we won't get the optimal code i.e.
        --
        --  let y = let x = e1
        --          in e2[x]
        --  in \z -> e3[y]
        --
        -- It seems really hard to work out a minimal set of things to add to the "always residualise" set every time. One promising approach is to:
        --  1. Construct a graph from the heap/stack/context to split with these nodes:
        --     a) x for (x |-> d) in heap: outgoing edges go to the update frames/heap bindings binding the free variables of d
        --     b) i for i in [1..length(stack)]: outgoing edges go to the update frames/heap bindings binding the free variables of stack_i
        --  2. Mark all the nodes in the graph with \infty occurrence that are *not* heap-bound value nodes
        --  3. Remove any marks that are strictly dominated by other marks along ALL of their incoming edges
        --
        -- The problem with this scheme is that the mark-removal can lead to ambiguity if more than one thing in a SCC is marked.  Consider:
        --
        --  let x = e1[y]
        --      y = e2[x]
        --      a = e3[x]
        --      b = e4[x]
        --  in (a, b)
        --
        -- In the first iteration, both a and b will be marked "always residualise" since the marks on x and y will be dominated by those on a and b.
        -- On the next iteration, both x and y will be marked again (they will both be trial-inlined into both the a and b bindings) and we have the
        -- choice of unmarking either of them but not both.
        --
        -- In this case the right thing to do is unmark "y" and leave "x" marked since unmarking "x" will just lead to it being residualised on the
        -- next iteration anyway, but unmarking "y" will allow it to be inlined into the x binding.
        inlineHeapWithKey :: (Deeds -> a                 -> (Deeds, EnteredEnv, b))
                          ->  Deeds -> M.Map (Out Var) a -> (Deeds, EnteredEnv, M.Map (Out Var) b)
        inlineHeapWithKey f deeds b = (deeds', overall_entered', b')
          where
            ((deeds', heap_entered'), b') = M.mapAccumWithKey (\(deeds, heap_entered) x' brack -> case f deeds brack of (deeds, entered', brack) -> ((deeds, M.insert x' entered' heap_entered), brack)) (deeds, M.empty) b

            overall_entered' = -- pprTrace "overall_entered'" (ppr (M.toList heap_entered')) $
                               foldr go emptyVarEnv $ stronglyConnCompG $ graphFromEdgedVertices [(entered', varUnique x', varEnvKeys entered') | (x', entered') <- M.toList heap_entered']

            go (AcyclicSCC (entered', _, _)) overall_entered = plusVarEnv_C plusEntered entered' overall_entered
            go (CyclicSCC  nodes)            overall_entered = foldr (\entered' overall_entered' -> plusVarEnv_C plusEntered (entered' `delListFromUFM_Directly` xs') overall_entered') overall_entered entereds'
              where (entereds', xs', _) = unzip3 nodes

        -- Inline what we can of the heap, and compute the Entered information for the resulting thing.
        -- See Note [transitiveInline and entered information] for the story about Entered information.
        --
        -- TODO: I (probably) need to transfer the EnteredEnv safely out of Bracketed things, taking account of bound variables
        -- over the holes. However, I think it's probably safe to ignore that for now because those bound variables will have been
        -- renamed so as not to coincide with any of the heap/stack bindings above that we actually care about the entered information for.
        -- So the outgoing entered envs will have a bit of junk in them, but who cares?
        inlineBracketHeap :: Deeds -> Bracketed (Entered, UnnormalisedState) -> (Deeds, EnteredEnv, RBracketed State)
        inlineBracketHeap init_deeds = third3 rigidizeBracketed . inlineHeapT inline_one init_deeds
          where
            inline_one deeds (ent, state) = -- pprTrace "inline_one" (ppr (ent, stateFreeVars state', state)) $
                                            (deeds', mkEnteredEnv ent $ stateFreeVars state', (emptyDeeds, heap', k', in_e'))
              where
                -- The elements of the Bracketed may contain proposed heap bindings gathered from Case frames.
                -- However, we haven't yet claimed deeds for them :-(.
                --
                -- This is OK, because transitiveInline treats the heap of its state "specially". NB: the correctness
                -- of this relies on the fact that only "optional" bindings that shadow something bound above are ever
                -- present in this heap.
                --
                -- We do the normalisation immediately afterwards - we can't do it before transitiveInline, precisely
                -- because of the above hack (normalisation might add bindings to the heap).
                state'@(deeds', heap', k', in_e') = normalise $ transitiveInline h_inlineable (deeds `addStateDeeds` state)
        
        -- 3c) Actually do the inlining of as much of the heap as possible into the proposed floats
        -- We also take this opportunity to strip out the Entered information from each context.
        (deeds3, entered_focus, bracketed_focus') =                   inlineBracketHeap deeds2 bracketed_focus
        (deeds4, entered_heap,  bracketeds_heap') = inlineHeapWithKey inlineBracketHeap deeds3 bracketeds_heap

        -- 4) Construct the next element of the fixed point process:
        --  a) We should residualise:
        --     * Any x in the extraFvs of a bracketed thing, because we need to be able to refer to it right here, whatever happens
        --     * Anything explicitly generalised
        must_resid_xs = bracketedExtraFvs bracketed_focus' `unionVarSet` unionVarSets (map bracketedExtraFvs (M.elems bracketeds_heap'))
                          `unionVarSet` gen_xs
        --  b) We should *stop* residualising bindings that got Entered only once in the proposal.
        --     I once thought that we should only add a single variable to non_resid_xs' every time around the loop, because I worried
        --     that choosing not to residualise some binding would cause some other bindings to stop being candiates (i.e. would increase
        --     the number of times they were entered).
        --
        --     However, I've revised my opinion and decided to add all candidate variables every time. This is because if we inline a binding
        --     into a context where it is still evaluated Once, anything it refers to is still evaluated Once. So the existing Entered information
        --     does not appear to be invalidated when we decide not to residualise an additional binding.
        entered    = plusVarEnv_C plusEntered entered_focus entered_heap
        safe_not_resid_xs' = -- pprTrace "candidates" (ppr (safe_not_resid_xs, entered, onces, must_resid_xs)) $
                             safe_not_resid_xs `unionVarSet` (onces `minusVarSet` must_resid_xs)
          where onces = filterVarSet (\x' -> maybe True isOnce (lookupVarEnv entered x')) bound_xs
        --   c) We should *start* residualising those bindings we thought were safe to inline but we actually couldn't inline because
        --      deeds issues prevented us from inlining them into *all* contexts that needed them. See also Note [Deeds and splitting]
        --
        --      This should also deal with residualising any InternallyBound stuff that we decided to instead let/lambda bound to e.g. prevent
        --      value duplication, because the names of such bound things will be free in the proposed states.
        deeds_resid_xs' = deeds_resid_xs `unionVarSet` (safe_not_resid_xs `intersectVarSet` (bracketedFreeVars stateFreeVars bracketed_focus' `unionVarSet`
                                                                                             unionVarSets (map (bracketedFreeVars stateFreeVars) (M.elems bracketeds_heap'))))
    
    -- Bound variables: those variables that I am interested in making a decision about whether to residualise or not
    bound_xs = pureHeapBoundVars h `unionVarSet` stackBoundVars (fmapCars snd named_k)
    
    -- Heap full of cheap expressions and any phantom stuff from the input heap but NOT from update frames
    -- Used within the main loop in the process of computing h_inlineable -- see comments there for the full meaning of this stuff.
    extract_cheap_hb hb
       -- We better not try to push down any bindings that would introduce work-duplication issues
      | InternallyBound <- howBound hb
      , Just (_, e) <- heapBindingTerm hb
      = if termIsCheap e
        then hb {                                         howBound = howToBindCheap e } -- Use binding heuristics to determine how to refer to the cheap thing
        else hb { heapBindingMeaning = Left (Left False), howBound = LambdaBound }      -- GHC is unlikely to get any benefit from seeing the binding sites for non-cheap things
       -- Inline phantom/unfolding stuff verbatim: there is no work duplication issue (the caller would not have created the bindings unless they were safe-for-duplication)
      | otherwise
      = hb
    h_cheap_and_phantom0 = M.map extract_cheap_hb h
    h_cheap_and_phantom | (_, Tagged _ (Update x')) `Car` _ <- named_k -- NB: by normalisation, there can't be a cast before the update
                        , Just anned_qa <- mb_anned_qa
                        , Right anned_a <- caseAnnedQA anned_qa -- FIXME: having a question here might also be legit
                        , let in_e@(_, e) = annedAnswerToInAnnedTerm anned_a
                        = M.insert x' ((internallyBound in_e) { howBound = howToBindCheap e }) h_cheap_and_phantom0
                        | otherwise
                        = h_cheap_and_phantom0

howToBindCheap :: AnnedTerm -> HowBound
howToBindCheap e
  | not lOCAL_TIEBACKS = InternallyBound
  | dUPLICATE_VALUES_SPLITTER = InternallyBound
  | Value v <- annee e = case v of
    TyLambda _ _ -> LetBound -- Heuristic: GHC would lose too much if we cut the
    Lambda   _ _ -> LetBound -- connection between the definition and use sites
    Data _ as cos xs | null as, null cos, null xs -> InternallyBound -- Heuristic: GHC will actually statically allocate data with no arguments (this also has the side effect of preventing tons of type errors due to [] getting shared)
                     | otherwise                  -> LambdaBound
    Literal _  -> InternallyBound -- No allocation duplication since GHC will float them (and common them up, if necessary)
    Coercion _ -> InternallyBound -- Not allocated at all
   -- GHC is unlikely to get anything useful from seeing the definition of cheap non-values, so we'll have them as unfoldings
  | otherwise = LambdaBound

-- Note [Deeds and splitting]
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~
-- Some heap bindings are safe to inline (from a work-duplication perspective), but bad to inline from a deeds perspective
-- because it can prove impossible to get enough deeds to actually inline them. We apply a rather unsubtle (but safe)
-- heuristic to deal with this situation, by monotonically growing a set of variables that we should *not* attempt
-- to inline even though they appear in the safe_not_resid_xs set.
-- 
-- This really is extremely conservative, but if we're running out of deeds bad things will happen anyway, so who cares?
--
-- If we did not do this, then the bracketed_heap outgoing from splitt may not bind some of the variables free in what
-- it intends to drive, because bracketeds_heap only contains those bindings that splitt decided should be residualised.


-- Note [Residualisation of things referred to in extraFvs]
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
-- We need to residualise stuff like the a and b in this:
--  <a |-> 1, b |-> 2 | (a, b) | >
--
-- But *not* the x in this:
--  < | foo | case foo of \Delta, update x, [_] + 1 >
--
-- How the hell do we accomplish that? The trick is to change how update frames get split. After splitting an
-- update frame for x, I continue splitting the rest of the stack with a oneBracketed rather than a noneBracketed in
-- the focus.
--
-- If I didn't do this, the extraFvs of the bracket would indicate that x was free in a created residual term, so I
-- would be forced to residualise the binding just as if e.g. "Just x" had been in the focus of the state. Since I don't,
-- x doesn't appear in the extraFvs, and I can compute Entered information for it with transitiveInline. If this says x
-- was entered Once in aggregate I can stop residualising the update frame! Beautiful!
--
-- FIXME: this buggered up when I started splitting terms like < | I# a# | update a > because they split to *themselves*
-- by pushing the recovered value heap { a |-> I# a# } into the oneBracketed "a" from the update frame. I only really need
-- this trick because I start by assuming that x is non-pushable, anyway, so I should probably rewrite the splitter
-- to use that new more agressive algorithm.. (I can remove the eager value splittin for Update frames too since that wasn't
-- causing the loops I was seeing - this was)
--
-- Note [Entered information]
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~
-- Consider:
--   expensive |-> fact 100
--   a |-> Just expensive
--   b |-> Just expensive
--   (a, b)
--
-- We need to residualise expensive, but what is the mechanism for doing so? Both a and b will get residualised
-- by the rule above because they are FVs of the focus.
--
-- We gather Entered information from each proposed Bracketed to collect Entered information for each free variable.
-- This reports how many times a variable would be (in the worst case) get reevaluated if the binding was made available
-- for inlining and thus pushed into that context. So in this case, Entered information for the a and b bindings report
-- that expensive would get evaluated Once in each context, which joins together to make Many times.
--
-- This is the basis on which we choose to residualise expensive.

-- We are going to use this helper function to inline any eligible inlinings to produce the expressions for driving.
--
-- WARNING! We treat bindings in the incoming Heap very specially we assume that we haven't yet claimed any deeds for them
--
-- This is a consequence of the fact that this heap is only non-empty in the splitter for states originating from the
-- branch of some residual case expression.
transitiveInline :: PureHeap          -- ^ What to inline. We have not claimed deeds for any of this.
                 -> UnnormalisedState -- ^ What to inline into
                 -> UnnormalisedState
transitiveInline init_h_inlineable _state@(deeds, Heap h ids, k, in_e)
    = -- (if not (S.null not_inlined_vs') then traceRender ("transitiveInline: generalise", not_inlined_vs') else id) $
      -- traceRender ("transitiveInline", "had bindings for", pureHeapBoundVars init_h_inlineable, "FVs were", state_fvs, "so inlining", pureHeapBoundVars h') $
      ASSERT2(isEmptyVarSet (unnormalisedStateUncoveredVars final_state), ppr (M.keysSet h_inlineable, PrettyDoc $ pPrintFullUnnormalisedState quietStatePrettiness _state, PrettyDoc $ pPrintFullUnnormalisedState quietStatePrettiness final_state, unnormalisedStateUncoveredVars final_state, M.keysSet h', live'))
      final_state
  where
    final_state = (deeds', Heap h' ids, k, in_e)
    
    (live', deeds', h') = heap_worker 0 deeds M.empty (unnormalisedStateFreeVars (deeds, Heap M.empty ids, k, in_e)) emptyVarSet
    
    -- NB: we prefer bindings from h to those from init_h_inlineable if there is any conflict. This is motivated by
    -- the fact that bindings from case branches are usually more informative than e.g. a phantom binding for the scrutinee.
    h_inlineable = h `M.union` init_h_inlineable
    
    -- This function is rather performance critical: I originally benchmarked transitiveInline as taking 59.2% of runtime for DigitsOfE2!
    heap_worker :: Int -> Deeds -> PureHeap -> FreeVars -> FreeVars -> (FreeVars, Deeds, PureHeap)
    heap_worker n deeds h_output live live_in_let
      = -- traceRender ("go", n, M.keysSet h_inlineable, M.keysSet h_output, fvs) $
        if live == live'
        then (live', deeds', neutraliseLetLives live_in_let' h_output') -- NB: it's important we use the NEW versions of h_output/deeds, because we might have inlined extra stuff even though live hasn't changed!
        else heap_worker (n + 1) deeds' h_output' live' live_in_let'
      where 
        (deeds', h_output', live', live_in_let') = M.foldrWithKey consider_inlining (deeds, h_output, live, live_in_let) ((h_inlineable `restrictDataMapVarSet` live) M.\\ h_output)
        
        -- NB: we rely here on the fact that our caller will still be able to fill in bindings for stuff from h_inlineable
        -- even if we choose not to inline it into the State, and that such bindings will not be evaluated until they are
        -- actually demanded (or we could get work duplication by inlining into only *some* Once contexts).
        --
        -- NB: we also rely here on the fact that the original h contains "optional" bindings in the sense that they are shadowed
        -- by something bound above - i.e. it just tells us how to unfold case scrutinees within a case branch.
        --
        -- NB: It's important that type variables become live after inlining a binding, or we won't
        -- necessarily lambda-abstract over all the free type variables of a h-function
        consider_inlining x' hb (deeds, h_output, live, live_in_let)
          = (deeds', M.insert x' inline_hb h_output, live `unionVarSet` fvs, if howBound inline_hb == LetBound then live_in_let `unionVarSet` fvs else live_in_let)
          where fvs = heapBindingFreeVars inline_hb `unionVarSet` varBndrFreeVars x'
                (deeds', inline_hb) = case claimDeeds deeds (heapBindingSize hb) of -- Do we have enough deeds to inline an unmodified version?
                  Just deeds' ->                                                       (deeds', hb)
                  Nothing     -> trace (showSDoc $ text "inline-deeds:" <+> pPrint x') (deeds,  makeFreeForDeeds hb)

    -- Given a HeapBinding that costs some deeds, return one that costs no deeds (and so can be inlined unconditionally)
    makeFreeForDeeds (HB InternallyBound (Right in_e))
      | not lOCAL_TIEBACKS     = lambdaBound         -- Without local tiebacks, we just lose information here
      | termIsCheap (snd in_e) = HB how (Right in_e) -- With local tiebacks, we can keep the RHS (perhaps we can use it in the future?) but have to make it be able to pass it in from the caller somehow
      | otherwise              = lambdaBound         -- All non-cheap things
      where how | termIsValue (snd in_e) = LetBound    -- Heuristic: only refer to *values* via a free variable, as those are the ones GHC will get some benefit from. TODO: make data/function distinction here?
                | otherwise              = LambdaBound
    makeFreeForDeeds hb = panic "howToBind: should only be needed for internally bound things with a term" (pPrint hb)
    
    -- Enforce the invariant that anything referred to by a LetBound thing cannot be LambdaBound
    neutraliseLetLives live_in_let = M.mapWithKey (\x' hb -> if howBound hb == LambdaBound && x' `elemVarSet` live_in_let then hb { howBound = LetBound } else hb)

-- TODO: replace with a genuine evaluator. However, think VERY hard about the termination implications of this!
-- I think we can only do it when the splitter is being invoked by a non-whistling invocation of sc.
cheapifyHeap :: Deeds -> Heap -> (Deeds, Heap)
cheapifyHeap deeds heap | sPECULATION = (deeds, heap)
cheapifyHeap deeds (Heap h ids) = (deeds', Heap (M.fromList [(x', internallyBound in_e) | (x', in_e) <- floats] `M.union` h') ids')
  where
    ((deeds', ids', floats), h') = M.mapAccum (\(deeds, ids, floats0) hb -> case hb of HB InternallyBound (Right in_e) -> (case cheapify deeds ids in_e of (deeds, ids, floats1, in_e') -> ((deeds, ids, floats0 ++ floats1), HB InternallyBound (Right in_e'))); _ -> ((deeds, ids, floats0), hb)) (deeds, ids, []) h
    
    -- TODO: make cheapification more powerful (i.e. deal with case bindings)
    cheapify :: Deeds -> InScopeSet -> In AnnedTerm -> (Deeds, InScopeSet, [(Out Var, In AnnedTerm)], In AnnedTerm)
    cheapify deeds0 ids0 (rn, anned_e)
      | LetRec xes e <- annee anned_e
      , let deeds1 = deeds0 `releaseDeeds` 1
            (        ids1, rn', in_xes) = renameBounds ids0 rn xes
            (in_xs, in_es) = unzip in_xes
            (deeds2, ids2, floats0, in_es') = cheapifyMany deeds1 ids1 in_es
            (deeds3, ids3, floats1, in_e')  = cheapify deeds2 ids2 (rn', e)
      = (deeds3, ids3, zip in_xs in_es' ++ floats0 ++ floats1, in_e')
      | Let x e1 e2 <- annee anned_e
      , let deeds1 = deeds0 `releaseDeeds` 1
            (ids1, rn', (x', in_e1)) = renameNonRecBound ids0 rn (x, e1)
            (deeds2, ids2, floats0, in_e1')  = cheapify deeds1 ids1 in_e1
            (deeds3, ids3, floats1, in_e2')  = cheapify deeds2 ids2 (rn', e2)
      = (deeds3, ids3, (x', in_e1') : floats0 ++ floats1, in_e2')
    cheapify deeds ids in_e = (deeds, ids, [], in_e)

    cheapifyMany :: Deeds -> InScopeSet -> [In AnnedTerm] -> (Deeds, InScopeSet, [(Out Var, In AnnedTerm)], [In AnnedTerm])
    cheapifyMany deeds ids = reassociate . mapAccumL ((associate .) . uncurry cheapify) (deeds, ids)
      where associate (deeds, ids, floats, in_e) = ((deeds, ids), (floats, in_e))
            reassociate ((deeds, ids), floatss_in_es) = (deeds, ids, concat floatss, in_es)
                where (floatss, in_es) = unzip floatss_in_es


-- TODO: I have a clever idea. Currently, if we supercompile:
--  D[ < H | if x then y else z | K > ]
--
-- And we don't know anything about y or z we get:
--  if x
--   then K(True/x)[y]
--   else K(False/x)[z]
--
-- This is not too bad, but I suspect that it is common that K doesn't actually depend on x, in which case we could
-- instead produce:
--  let $j it = K[it]
--  in if x then $j y else $j z
--
-- This is an improvement because we get code sharing. Furthermore, $j won't be causing extra allocation because it's
-- pretty much guaranteed to be a let-no-escape.
--
-- The best part is that making this happen isn't really much much work (I think). One option would be to actually add
-- a (JoinPoint Var) stack frame, and introduce them (along with their corresponding bindings) in the splitter. The reduction
-- rule would be:
--   < H | v | $j [_], K > --> < H, x |-> v | e | K >
--      \x.e = deref(H, $j)
--
-- If we said join points were LetBound this would also let us delay inlining them (and hence consuming deeds) until we
-- were sure we could get some benefit from it.
--
-- The major issue is exactly what *should* be bound up into a join point. We could create one per stack frame, but that
-- might lead to quite a lot of code bloat. I think that ideally we want to create one per shared stack suffix: there is no
-- point creating join points that are only used in one place! But how to detect that?? After all, because h-functions can
-- be tied back to at any later point it looks like we should create one for every possible prefix as they might be useful
-- for guys in the future.
pushStack :: UniqSupply
          -> InScopeSet
          -> Deeds
          -> [Out Var]
          -> Train (Bool, Tagged StackFrame) Generalised
          -> Bracketed (Entered, UnnormalisedState)
          -> (Deeds,
              M.Map (Out Var) (Bracketed (Entered, UnnormalisedState)),
              Bracketed (Entered, UnnormalisedState))
pushStack _        _   deeds _      (Loco gen)               bracketed_hole = (deeds, M.empty, setStackGeneralised gen bracketed_hole)
pushStack ctxt_ids ids deeds scruts ((may_push, kf) `Car` k) bracketed_hole = second3 (`M.union` bracketed_heap') $ pushStack ctxt_ids2 ids deeds' scruts' k bracketed_hole'
  where
    (ctxt_ids1, ctxt_ids2) = splitUniqSupply ctxt_ids

    -- If we have access to hole tail positions, we should try to inline this stack frame into that tail position.
    -- If we do not have access to the tail positions of the hole, all we can do is rebuild a bit of residual syntax around the hole.
    (deeds', (scruts', bracketed_heap', bracketed_hole'))
      | may_push  = fmap (\(deeds', bracketed_hole') -> (deeds', ([], M.empty, bracketed_hole'))) (pushStackFrame kf deeds bracketed_hole) `orElse`
                    (deeds, splitStackFrame ctxt_ids1 ids kf scruts                           bracketed_hole)
      | otherwise = (deeds, splitStackFrame ctxt_ids1 ids kf scruts (setStackGeneralised True bracketed_hole))

setStackGeneralised :: Generalised -> Bracketed (Entered, UnnormalisedState) -> Bracketed (Entered, UnnormalisedState)
setStackGeneralised gen bracketed_hole = modifyTails_ id (map (second (third4 (fmapLoco (\_old_gen -> gen))))) bracketed_hole `orElse` bracketed_hole

pushStackFrame :: Tagged StackFrame
               -> Deeds
               -> Bracketed (Entered, UnnormalisedState)
               -> Maybe (Deeds, Bracketed (Entered, UnnormalisedState))
pushStackFrame kf deeds bracketed_hole = do
    (Just deeds', bracketed_hole') <- modifyTails (stackFrameType kf) push bracketed_hole
    return (deeds', bracketed_hole')
  where
    -- Inline parts of the evaluation context into each branch only if we can get that many deeds for duplication
    push :: [(Entered, UnnormalisedState)] -> (Maybe Deeds, [(Entered, UnnormalisedState)])
    push fillers = case claimDeeds deeds (stackFrameSize (tagee kf) * (branch_factor - 1)) of -- NB: subtract one because one occurrence is already "paid for". It is OK if the result is negative (i.e. branch_factor 0)!
            Nothing    -> trace (showSDoc $ text "pushStack-deeds" <+> pPrint branch_factor) (Nothing, fillers)
            Just deeds ->                                                                    (Just deeds, map (\(ent, state) -> (ent, third4 (`trainAppend` \_gen -> kf `Car` Loco False) state)) fillers)
      where branch_factor = length fillers

{-
data Scrutinee = Scrut {
    scrutAliases    :: [Out Var],
    scrutBottomness :: Maybe Int -- Number of value arguments remaining until scrutinee is _|_
  }

-- The scrutinee bottomness check was introduced to deal with the fact that genregexps was
-- compiling lots of specialisations of the *error case* of "succ" and "expand".
--
-- You might think that this wouldn't do much harm, because in the error case we have just
-- as much information about the scrutinee as when it was totally unknown (e.g. when it is
-- succ of an unknown quantity). In this case we would expect the unspecialised supercompiled
-- context in which the errors occur would be reusable at other sites where the scrutinee was unknown.
--
-- Unfortunately, this is *not* the case because the normal case of "succ @Char" is to return a C#
-- box, but the error case returns an error outside of a C# box. FIXME: this is still not much change,
-- just need one extra residual stack frame to unpack the box before we reach shared code.
--
-- Note that this check only really helps functions for which we do not know the RHS (e.g. primops).
-}

splitStackFrame :: UniqSupply
                -> InScopeSet
                -> Tagged StackFrame
                -> [Out Var]
                -> Bracketed (Entered, UnnormalisedState)
                -> ([Out Var],
                    M.Map (Out Var) (Bracketed (Entered, UnnormalisedState)),
                    Bracketed (Entered, UnnormalisedState))
splitStackFrame ctxt_ids ids kf scruts bracketed_hole
  | Update x' <- tagee kf = splitUpdate ctxt_ids ids tg scruts x' bracketed_hole
  | otherwise = ([], M.empty, case tagee kf of
    Update x' -> pprPanic "splitStackFrame" (text "Encountered update frame for" <+> pPrint x' <+> text "that was handled above")
    TyApply ty' -> zipBracketeds $ TailsUnknown (shell (tyVarsOfType ty') $ \[e] -> e `tyApp` ty') [Hole [] bracketed_hole]
    CoApply co' -> zipBracketeds $ TailsUnknown (shell (tyCoVarsOfCo co') $ \[e] -> e `coApp` co') [Hole [] bracketed_hole]
    Apply x2'   -> zipBracketeds $ TailsUnknown (shell (unitVarSet x2')   $ \[e] -> e `app` x2')   [Hole [] bracketed_hole]
    CastIt co'  -> zipBracketeds $ TailsUnknown (shell (tyCoVarsOfCo co') $ \[e] -> e `cast` co')  [Hole [] bracketed_hole]
    Scrutinise x' ty' (rn, unfiltered_alts)
      -> -- (if null k_remaining then id else traceRender ("splitStack: FORCED SPLIT", M.keysSet entered_hole, [x' | Tagged _ (Update x') <- k_remaining])) $
         -- (if not (null k_not_inlined) then traceRender ("splitStack: generalise", k_not_inlined) else id) $
         zipBracketeds $ TailsKnown ty' (\final_ty' -> shell (tyVarsOfType final_ty') $ \(e_hole:es_alts) -> case_ e_hole x' final_ty' (alt_cons' `zip` es_alts)) (TailishHole False (Hole [] bracketed_hole) : zipWithEqual "Scrutinise" (\alt_bvs -> TailishHole True . Hole (x':alt_bvs)) alt_bvss bracketed_alts)
      where -- These lines achieve two things:
            --   1. Filter out any branches of the case which we know are impossible due to type refinement
            --   2. Turn any remaining default cases into explicit constructors if possible (helps positive information propagation)
            alts = [ (coreAltConToAltCon altcon xs, e)
                   | (altcon, xs, e) <- (if rEFINE_ALTS then thirdOf3 . filterAlts (repeat wildCardKey) (idType x') [] else id)
                                            [(altcon', xs, e) | (altcon, e) <- unfiltered_alts, let (altcon', xs) = altConToCoreAltCon altcon]
                   ]

            (alt_cons, alt_es) = unzip alts
            scruts' = x':scruts

            -- 0) Manufacture context identifier
            ctxt_id = uniqFromSupply ctxt_ids
            
            -- 1) Construct the floats for each case alternative
            -- We have to carefully zap OccInfo here because one of the case binders might be marked as dead,
            -- yet could become live due to positive information propagation!    
            (alt_idss, alt_rns, alt_cons') = unzip3 $ map (renameAltCon ids rn) $ if any (not . isDeadBinder) scruts'
                                                                                   then map zapAltConIdOccInfo alt_cons
                                                                                   else alt_cons
            -- Bind something to the case scrutinee (if possible). This means that:
            --  let y = (\z -> case z of C -> ...) unk
            --  in y
            -- ===>
            --  case x of C -> let unk = C; z = C in ...
            alt_in_es = alt_rns `zip` alt_es
            alt_hs = zipWithEqual "alt_hs" (\alt_con' alt_bvs -> M.fromList (do guard pOSITIVE_INFORMATION
                                                                                Just scrut_v <- [altConToValue (idType x') alt_con']
                                                                                let in_scrut_e@(_, scrut_e) = renamedTerm (fmap Value scrut_v)
                                                                                scrut <- scruts'
                                                                                -- Localise the Id just in case this is the occurrence of a lambda-bound variable.
                                                                                -- We don't really want a Let-bound external name in the output!
                                                                                return (localiseId scrut, HB (howToBindCheap scrut_e) (Right in_scrut_e)))
                                                                 `M.union` M.fromList [(x, lambdaBound) | x <- x':alt_bvs]) -- NB: x' might be in scruts and union is left-biased
                                           alt_cons' alt_bvss -- NB: don't need to grab deeds for these just yet, due to the funny contract for transitiveInline
            alt_bvss = map altConBoundVars alt_cons'
            bracketed_alts = zipWith3Equal "bracketed_alts" (\alt_h alt_ids alt_in_e -> oneBracketed' ty' (Once ctxt_id, (emptyDeeds, Heap alt_h alt_ids, Loco False, alt_in_e))) alt_hs alt_idss alt_in_es
    StrictLet x' in_e -> zipBracketeds $ TailsKnown ty' (\_final_ty' -> shell emptyVarSet $ \[e_hole, e_body] -> let_ x' e_hole e_body) [TailishHole False $ Hole [] bracketed_hole, TailishHole True $ Hole [x'] $ oneBracketed' ty' (Once ctxt_id, (emptyDeeds, Heap (M.singleton x' lambdaBound) ids, Loco False, in_e))]
      where ctxt_id = uniqFromSupply ctxt_ids
            ty' = inTermType ids in_e
    PrimApply pop tys' in_vs in_es -> zipBracketeds $ TailsUnknown (shell emptyVarSet $ primOp pop tys') (zipWith Hole (repeat []) $ bracketed_vs ++ bracketed_hole : bracketed_es)
      where -- 0) Manufacture context identifier (actually, an infinite number of them)
            (ctxt_ids0, ctxt_ids1) = splitUniqSupply ctxt_ids
            ctxt_idss0 = listSplitUniqSupply ctxt_ids0
            ctxt_idss1 = listSplitUniqSupply ctxt_ids1
            
            -- 1) Split every value and expression remaining apart
            bracketed_vs = zipWith (\ctxt_ids in_v -> splitCoerced (splitAnswer ctxt_ids ids) (annedToTagged in_v)) ctxt_idss0 in_vs
            bracketed_es  = zipWith (\ctxt_ids in_e -> let (ctxt_id, ctxt_ids0) = takeUniqFromSupply ctxt_ids in oneBracketed ctxt_ids0 (inTermType ids in_e) (Once ctxt_id, (Heap M.empty ids, Loco False, in_e))) ctxt_idss1 in_es)
  where
    tg = tag kf
    shell = Shell (oneResidTag tg)

-- I'm making use of a clever trick: after splitting an update frame for x, instead of continuing to split the stack with a
-- noneBracketed for x in the focus, I split the stack with a oneBracketed for it in the focus.
--
-- You might think this is utterly worthless, since by definition the splitter will never be able to push the actual definition of
-- x into this hole in the bracketed. However, the fact that we do this is *critical* to the algorithm I use to ensure that
-- we can make variables bound by update frames as non-residualised: see Note [Residualisation of things referred to in extraFvs]
splitUpdate :: UniqSupply -> InScopeSet -> Tag -> [Out Var] -> Var -> Bracketed (Entered, UnnormalisedState)
            -> ([Out Var], M.Map (Out Var) (Bracketed (Entered, UnnormalisedState)), Bracketed (Entered, UnnormalisedState))
splitUpdate ctxt_ids ids tg_kf scruts x' bracketed_hole
  = (x' : scruts, M.singleton x' bracketed_hole,
     oneBracketed ctxt_ids (idType x') (Once ctxt_id, (Heap M.empty ids, Loco False, (mkIdentityRenaming (unitVarSet x'), annedTerm tg_kf (Var x')))))
  where ctxt_id = idUnique x'

splitValue :: UniqSupply -> InScopeSet -> Tag -> In AnnedValue -> Bracketed (Entered, UnnormalisedState)
splitValue ctxt_ids ids tg (rn, Lambda x e)   = splitLambdaLike Lambda   ent            ctxt_ids0 ids tg (rn, (x, e))
  where -- This is really necessary if we want to fuse a top-level non-value with some consuming context in the IO monad
        --
        -- NB: this is rather interesting. If I have:
        --   x = e1
        --   y = v2[x]
        --   z = \a -> e3[y]
        --
        -- If I start supercompiling with y as the root, I might be able to fuse e1 into v2 if the occurrence
        -- within the RHS of y is Once. However, if I start from z then I may not be able to do this fusion if
        -- y occurs in a Many context because the value portion v2 will be moved down and the expression portion
        -- x will be left residualised above the lambda.
        --
        -- What I gain from this behaviour, of course, is that v2 may fuse with e3, which is probably more valuable
        -- in general anyway. 
        (ent, ctxt_ids0) | isOneShotBndr x = first Once $ takeUniqFromSupply ctxt_ids
                         | otherwise       = (Many, ctxt_ids)
splitValue ctxt_ids ids tg (rn, TyLambda a e) = splitLambdaLike TyLambda (Once ctxt_id) ctxt_ids0 ids tg (rn, (a, e))
  where (ctxt_id, ctxt_ids0) = takeUniqFromSupply ctxt_ids
splitValue _        ids tg in_v               = noneBracketed tg (value (annedValueToFVedValue' $ renameIn (renameAnnedValue' ids) in_v))

-- We create LambdaBound entries in the Heap for both type and value variables, so we can share the code:
splitLambdaLike :: (Var -> FVedTerm -> ValueF FVed) -> Entered
                -> UniqSupply -> InScopeSet -> Tag -> In (Var, AnnedTerm) -> Bracketed (Entered, UnnormalisedState)
splitLambdaLike rebuild entered ctxt_ids ids tg (rn, (x, e)) = zipBracketeds $ TailsUnknown (Shell (oneResidTag tg) emptyVarSet $ \[e'] -> value (rebuild x' e')) [Hole [x'] $ oneBracketed ctxt_ids (inTermType ids' in_e) (entered, (Heap (M.singleton x' lambdaBound) ids', Loco False, in_e))]
  where (ids', rn', x') = renameNonRecBinder ids rn x
        in_e = (rn', e)

splitCoerced :: (Tagged a -> Bracketed (Entered, UnnormalisedState))
             -> Tagged (Coerced a) -> Bracketed (Entered, UnnormalisedState)
splitCoerced f (Tagged tg_a  (Uncast,         a)) = f (Tagged tg_a a)
splitCoerced f (Tagged tg_co (CastBy co tg_a, a)) = zipBracketeds $ TailsUnknown (Shell (oneResidTag tg_co) (tyCoVarsOfCo co) $ \[e'] -> cast e' co) [Hole [] (f (Tagged tg_a a))]
 -- NB: the tg' in the CastBy is wrapped around the *x*, not the whole cast, so pass it down

splitQA :: UniqSupply -> InScopeSet -> Tag -> QA -> Bracketed (Entered, UnnormalisedState)
splitQA _        _   tg (Question x') = noneBracketed tg (var x')
splitQA ctxt_ids ids tg (Answer a)    = splitAnswer ctxt_ids ids (Tagged tg a)

splitAnswer :: UniqSupply -> InScopeSet -> Tagged Answer -> Bracketed (Entered, UnnormalisedState)
splitAnswer ctxt_ids ids (Tagged tg a) = splitValue ctxt_ids ids tg a

inTermType :: InScopeSet -> In AnnedTerm -> Type
inTermType ids = renameIn (renameType ids) . fmap termType
