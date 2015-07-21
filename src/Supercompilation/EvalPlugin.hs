{-# LANGUAGE GeneralizedNewtypeDeriving, ScopedTypeVariables, TupleSections #-}

module Supercompilation.EvalPlugin (evalPluginPass) where

import Control.Monad.State
import Data.Bifunctor
import qualified Data.IntMap as IM
import Data.IORef
import Data.Maybe

import HERMIT.Core

import CoreMonad
import CoreSyn
import CoreUnfold
import GhcPlugins
import IdInfo
import SimplEnv
import Simplify
import SimplMonad
import Unique (getKey)

import Supercompilation.Show

type Unfoldings = IM.IntMap Unfolding

newtype PE a = PE { unwrapPE :: StateT PEState CoreM a }
  deriving (Functor, Applicative, Monad, MonadState PEState, MonadIO)

instance HasDynFlags PE where
  getDynFlags = PE $ lift getDynFlags

getModGuts :: PE ModGuts
getModGuts = gets peGuts

lift' = PE . lift

data PEState = PEState
  { peGuts :: ModGuts
  }

runPE :: ModGuts -> PE a -> CoreM a
runPE guts (PE pe) = evalStateT pe (PEState guts)

evalPluginPass :: ModGuts -> CoreM ModGuts
evalPluginPass guts = do
    getDynFlags >>= liftIO . writeIORef dynFlags_ref

    let binds = mg_binds guts
    let unfoldings     = concatMap makeUnfolding binds
        unfoldings_map :: Unfoldings = unfoldingsMap unfoldings

    mapM_ showUnfolding unfoldings

    -- binds' <- runPE guts $ mapM (evalDef unfoldings_map) binds
    let binds' = binds

    return guts{ mg_binds = binds' }

makeUnfolding :: CoreBind -> [(CoreBndr, Unfolding)]
makeUnfolding (NonRec b rhs) =
    [(b, mkCoreUnfolding InlineRhs True rhs (UnfWhen (arity rhs) False True))]
makeUnfolding (Rec bs) = concatMap (\(b, rhs) -> makeUnfolding (NonRec b rhs)) bs

unfoldingsMap :: [(CoreBndr, Unfolding)] -> Unfoldings
unfoldingsMap = IM.fromList . map (\(b, u) -> (coreBindKey b, u))

showUnfolding :: (CoreBndr, Unfolding) -> CoreM ()
showUnfolding (b, u) = liftIO $ do
    putStrLn $ "unfolding for " ++ showOutputable b ++ " is " ++ showOutputable u
    putStrLn $ "  template: " ++ showOutputable (maybeUnfoldingTemplate u)

arity :: CoreExpr -> Int
arity (Lam _ body) = 1 + arity body
arity _            = 0

evalDef :: Unfoldings -> CoreBind -> PE CoreBind
evalDef ufs (NonRec b rhs) =
    NonRec b <$> evalDef_expr ufs rhs
evalDef ufs (Rec bs)       =
    Rec <$> mapM (\(b, rhs) -> (b,) <$> evalDef_expr ufs rhs) bs

evalDef_expr :: Unfoldings -> CoreExpr -> PE CoreExpr
evalDef_expr _ e@Var{} = return e
evalDef_expr _ e@Lit{} = return e
evalDef_expr _ e@Lam{} = return e
evalDef_expr ufs e@App{} = do
    liftIO $ putStrLn $ "beta reduction: " ++ showOutputable e
    let (fn, args) = first (flip ufVarTemplate_rec ufs) $ collectArgs e
    liftIO $ putStrLn $ "before: " ++ showOutputable (mkApps fn args)
    let (fn', args') = betaReduceAll fn args
        e' = mkApps fn' args'
    liftIO $ putStrLn $ "after: " ++ show e'
    return e'

evalDef_expr ufs (Let bs body) = do
    bs' <- evalDef ufs bs
    let ufs' :: Unfoldings = unfoldingsMap (makeUnfolding bs') `IM.union` ufs
    Let bs' <$> evalDef_expr ufs body

-- evalDef_expr ufs (Case scrt b ty alts) = do
--     scrt' <- evalDef_expr ufs scrt
--     alts' <- mapM (evalDef_alt ufs) alts
--     return $ Case scrt' b ty alts'

evalDef_expr ufs (Cast e c) = flip Cast c <$> evalDef_expr ufs e
evalDef_expr ufs (Tick t e) = Tick t <$> evalDef_expr ufs e
evalDef_expr _ t@Type{} = return t
evalDef_expr _ c@Coercion{} = return c

evalDef_alt :: Unfoldings -> CoreAlt -> PE CoreAlt
evalDef_alt ufs (con, bs, rhs) = (con, bs,) <$> evalDef_expr ufs rhs


-- -- | Unfold variables in a bottom-up way.
-- unfoldTemplates :: CoreExpr -> Unfoldings -> CoreExpr
-- unfoldTemplates (Var v) ufs = fromMaybe (Var v) $ ufTemplate v ufs
-- unfoldTemplates e@Lit{} _   = e

ufVarTemplate_rec :: CoreExpr -> Unfoldings -> CoreExpr
ufVarTemplate_rec e@(Var v) ufs = maybe e (flip ufVarTemplate_rec ufs) $ ufTemplate v ufs
ufVarTemplate_rec e         _   = e

ufTemplate :: Id -> Unfoldings -> Maybe CoreExpr
ufTemplate i ufs =
    maybe (IM.lookup (coreBindKey i) ufs >>= maybeUnfoldingTemplate) Just $
      maybeUnfoldingTemplate (idUnfolding i)

--------------------------------------------------------------------------------
-- * Utils

coreBindKey :: CoreBndr -> Int
coreBindKey = getKey . getUnique

orElse :: Maybe a -> Maybe a -> Maybe a
x `orElse` y = case x of
                 Just _  -> x
                 Nothing -> y
