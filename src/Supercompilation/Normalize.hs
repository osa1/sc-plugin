{-# LANGUAGE FlexibleContexts #-}

module Supercompilation.Normalize where

import CoreMonad
import CoreSyn
import HscTypes
import Panic

import HERMIT.Context
import HERMIT.Core
import HERMIT.Dictionary.Local.Let
import HERMIT.Kure
import HERMIT.Monad

-- opt = repeatR $ anybuR $ letFloatExprR <+ letElimR

-- normalizeExpr :: ModGuts -> CoreExpr -> CoreM CoreExpr
-- normalizeExpr :: Injection CoreExpr a => ModGuts -> a -> CoreM a
-- normalizeExpr guts expr =
--     runHermitPass guts (promoteR $ repeatR $ anybuR $ letFloatExprR <+ letElimR) expr
-- 
-- runHermitPass :: ModGuts -> Rewrite HermitC HermitM a -> a -> CoreM a
-- runHermitPass guts r a =
--     runHM hermitEnv extractRet reportErr (applyR r (topLevelHermitC guts) a)
--   where
--     hermitEnv :: HermitMEnv
--     hermitEnv = mkEnv debugChan guts mempty
-- 
--     debugChan = const (return ()) -- we don't log debug messages for now
-- 
--     extractRet :: HermitMResult a -> CoreM a
--     extractRet = return . hResult -- TODO: in what cases do we have lemmas here?
-- 
--     reportErr :: String -> CoreM a
--     reportErr = panic
