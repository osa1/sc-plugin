{-# LANGUAGE FlexibleInstances, StandaloneDeriving, TypeSynonymInstances #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

-- | We define lots of orphan Show instances here, for debugging and learning
-- purposes.
--
-- Most of the time while trying to figure out when a constructor is used or how
-- is a term compiled, it's easiest to just create an example and run the plugin
-- on it.
--
-- Without Show instances though, we can't easily inspect compiled outputs.
-- Outputable generated strings hide lots of details(especially constructors),
-- but we still export a `showOutputable` here, for similar reasons.
--
module Supercompilation.Show where

import Data.IORef
import System.IO.Unsafe (unsafePerformIO)

import CostCentre
import GhcPlugins
import TypeRep

--------------------------------------------------------------------------------

{-# NOINLINE dynFlags_ref #-}
dynFlags_ref :: IORef DynFlags
dynFlags_ref = unsafePerformIO (newIORef undefined)

{-# NOINLINE dynFlags #-}
dynFlags :: DynFlags
dynFlags = unsafePerformIO (readIORef dynFlags_ref)

showOutputable :: Outputable a => a -> String
showOutputable = showSDoc dynFlags . ppr

--------------------------------------------------------------------------------
-- Orphan Show instances

deriving instance Show a => Show (Expr a)
deriving instance Show Type
deriving instance Show Literal
deriving instance Show a => Show (Tickish a)
deriving instance Show a => Show (Bind a)
deriving instance Show AltCon
deriving instance Show TyLit
deriving instance Show FunctionOrData
deriving instance Show Module
deriving instance Show CostCentre
deriving instance Show Role
deriving instance Show LeftOrRight
deriving instance Show IsCafCC

instance Show Var where
    show = show . varName

instance Show DataCon where
    show = show . dataConName

instance Show TyCon where
    show = show . tyConName

instance Show ModuleName where
    show = show . moduleNameString

instance Show PackageKey where
    show = show . packageKeyString

instance Show Name where
    show = showOutputable . nameOccName

-- deriving instance Show Name
-- deriving instance Show OccName

instance Show Coercion where
    show _ = "<Coercion>"


-- Instance for non-terms related stuff.

deriving instance Show CoreToDo
deriving instance Show SimplifierMode
deriving instance Show CompilerPhase
deriving instance Show FloatOutSwitches

instance Show PluginPass where
    show _ = "PluginPass"
