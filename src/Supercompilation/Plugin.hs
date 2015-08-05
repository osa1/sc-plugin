{-# LANGUAGE GeneralizedNewtypeDeriving, ScopedTypeVariables, TupleSections #-}

module Supercompilation.Plugin where

import qualified Control.Monad.State as S
import qualified Data.IntMap as IM
import qualified Data.IntSet as IS
import Data.IORef
-- import Data.Maybe (fromMaybe)
-- import Safe (headMay)

-- I don't like how GHC doesn't qualify it's module names..
import CoreMonad
import CoreSyn
import DynFlags
import GhcPlugins hiding (parens, split)
import Unique (getKey)

import Supercompilation.ANormal
import Supercompilation.Show
import Supercompilation.TaggedExpr (tagPgm, untagPgm, Tag)

--------------------------------------------------------------------------------

plugin :: Plugin
plugin = trace "running plugin" defaultPlugin { installCoreToDos = coreToDo }

coreToDo :: [CommandLineOption] -> [CoreToDo] -> CoreM [CoreToDo]
coreToDo _ todos = do
    liftIO $ putStrLn $ "TODOs: " ++ show todos
    -- TODO: Our transformations causing a panic in coreToStgExpr if we in run
    -- the pass as last thing to do. We should probably find a sweet-spot where
    -- we have nicely prepared CoreProgram and our passes don't invalidate GHC's
    -- invariants.
    -- injectPlugin (CoreDoPluginPass "supercompile" scPluginPass) todos
    return $ CoreDoPluginPass "supercompile" scPluginPass : todos
  where
    -- TODO: We have a problem here: CorePrep is never pushed to the pipeline.
    -- GHC's doCorePass isn't even handling CorePrep case. It's hard coded just
    -- before code generation step. It seems like currently there's no way to
    -- run a plugin pass after CorePrep.
    -- injectPlugin p [] = do
    --   liftIO $ putStrLn "Can't find CorePrep step in TODOs, injecting plugin as last thing to do."
    --   return [p]
    -- injectPlugin p (CorePrep : rest) = return (CorePrep : p : rest)
    -- injectPlugin p (coreStep : rest) = (coreStep :) <$> injectPlugin p rest

--------------------------------------------------------------------------------

scPluginPass :: ModGuts -> CoreM ModGuts
scPluginPass guts = do
    getDynFlags >>= liftIO . writeIORef dynFlags_ref
    -- convert to A-normal form
    -- hscEnv <- getHscEnv
    -- -- Why is this in IO???
    -- let modLoc = fromMaybe (panic "scPluginPass: Can't find current module in ModGuts.") $
    --                findCurrentModuleLoc (hsc_mod_graph hscEnv)
    -- pgm' <- liftIO $ corePrepPgm hscEnv modLoc (mg_binds guts) (mg_tcs guts)

    -- we should dump our progress here.

    -- return guts{ mg_binds = pgm' }

    -- Try tagging and untagging
    let tag = tagPgm (mg_binds guts)
        untag = untagPgm tag

    return guts{ mg_binds = untag }
  -- where
  --   findCurrentModuleLoc :: ModuleGraph -> Maybe ModLocation
  --   findCurrentModuleLoc graph =
  --     ms_location <$> headMay (filter ((==) (mg_module guts) . ms_mod) graph)

--------------------------------------------------------------------------------

type State  = (Heap, (Tag, QA), Stack)
type UState = (Heap, CoreExpr , Stack)

type Heap = IM.IntMap (Tag, CoreExpr)

-- NOTE: We may need to use CoreExpr for both cases, and use some smart
-- constructor functions.
data QA = -- Question Var | Answer Value
          Question Var | Answer CoreExpr

type Stack = [StackFrame]

type StackFrame = (Tag, UStackFrame)

data UStackFrame
  = Update Var Type     -- TODO: Do we really need a Type here?
  | Supply Var          -- ^ Supply the argument to function value
  | Instantiate Type    -- ^ Instantiate value
  | Case [CoreAlt]      -- TODO: Is using CoreAlt here a good idea?

data ScpState = ScpState
  { ssGuts     :: ModGuts
  , ssDynFlags :: DynFlags
  }

newtype ScpM a = ScpM { unwrapScpM :: S.State ScpState a }
  deriving (Functor, Applicative, Monad, S.MonadState ScpState)

instance HasDynFlags ScpM where
  getDynFlags = S.gets ssDynFlags

--------------------------------------------------------------------------------
-- * Main routiunes

sc, sc' :: History -> State -> ScpM CoreExpr

sc hist = memo (sc' hist)

sc' hist s =
  case terminate hist s of
    Stop           -> split (sc hist ) s
    Continue hist' -> split (sc hist') (reduce s)

reduce :: State -> State
reduce = go emptyHistory
  where
    go hist s =
      case terminate hist s of
        Stop           -> s
        Continue hist' ->
          maybe s (go hist' . normalize) $ step s

normalize :: UState -> State
normalize = undefined

step :: State -> Maybe UState
step = undefined

split :: Monad m -- an arbitrary monad? really?
      => (State -> m CoreExpr)
      -> (State -> m CoreExpr)
split = undefined

--------------------------------------------------------------------------------
-- * Termination check

-- TODO: this is isomorphic to Maybe History, if we could switch to that we can
-- use some nice combinators like `maybe`, `fromMaybe` etc.
data TermRes = Stop | Continue History

type History = [State]

emptyHistory :: History
emptyHistory = []

terminate :: History -> State -> TermRes
terminate prevs here
  | any (`wqo` here) prevs = Stop
  | otherwise              = Continue (here : prevs)

wqo :: State -> State -> Bool
wqo s1 s2 = tagBag s1 `wqo_bag` tagBag s2

type TagBag = [Int]

wqo_bag :: TagBag -> TagBag -> Bool
wqo_bag s1 s2 = IS.fromList s1 == IS.fromList s2 && length s1 <= length s2

tagBag :: State -> TagBag
tagBag (heap, (termTag, _), k) = tagBag_heap heap ++ (termTag : tagBag_cont k)

tagBag_heap :: Heap -> TagBag
tagBag_heap heap = map fst $ IM.elems heap

tagBag_cont :: Stack -> TagBag
tagBag_cont = map fst

--------------------------------------------------------------------------------
-- * Memoization

memo :: (State -> ScpM CoreExpr) -> (State -> ScpM CoreExpr)
memo = undefined

match :: State -> State -> Maybe Subst
match = undefined

{-

    anns <- getAnnotations deserializeWithData guts



    let knownVars :: [Name] =
          collectKnownConstructors (mg_tcs guts) ++ collectKnownDefs (mg_binds guts)
    -- let knownVars :: [(String, String)] = map (moduleNameString $ moduleName $ mg_module guts,) $
    --       collectKnownConstructors (mg_tcs guts) ++ collectKnownDefs (mg_binds guts)

    liftIO $ putStrLn $ "Known vars: " ++ show knownVars

    let binderInfo :: InfoTable = either error id $ buildInfoTable anns (mg_binds guts)

    -- This was for testing purposes, disabling for now
    -- liftIO $ putStrLn "Checking for type class dictionaries."
    -- mapM_ checkTCDicts (mg_binds guts)
    -- liftIO $ putStrLn "Done."

    let binds = mg_binds guts
    -- I'm wondering what are trade-offs between marking everything as dynamic
    -- at first vs. marking everything as static. As long as the result is
    -- congruent I think both are OK.
    --
    -- Here we start with everything is dynamic.
    let initSet = S.fromList knownVars
    statics <- congruence (\s bs -> foldM reportStatics s bs) initSet binds
                 =<< (foldM reportStatics initSet binds)
    liftIO $ putStrLn $ "statics: " ++ show statics
    -- binds <- mapM (printDecl binderInfo) (mg_binds guts)



    return guts{ mg_binds = binds }

collectKnownConstructors :: [TyCon] -> [Name]
collectKnownConstructors [] = []
collectKnownConstructors (tyc : rest)
  | isAlgTyCon tyc
  , DataTyCon cons _ <- algTyConRhs tyc
  , let conNames = map dataConName cons
  = conNames ++ collectKnownConstructors rest

  | otherwise
  = -- FIXME: Collect other types of constructors
    collectKnownConstructors rest

collectKnownDefs :: [CoreBind] -> [Name]
collectKnownDefs [] = []
collectKnownDefs (NonRec b _ : rest) = varName b : collectKnownDefs rest
collectKnownDefs (Rec bs : rest) = map (varName . fst) bs ++ collectKnownDefs rest

--------------------------------------------------------------------------------

data Condition
  = Trivial -- always holds, uncondition
  deriving (Show, Eq)

data InfoTable = InfoTable
  { it_terminateConditions :: IM.IntMap Condition
  } deriving (Show)

parseAnnotations :: [PEAnnotation] -> Either String Condition
parseAnnotations _ = return Trivial -- FIXME

buildInfoTable :: UniqFM [PEAnnotation] -> [CoreBind] -> Either String InfoTable
buildInfoTable anns bs = mapM parseAnnotations (collectConditions anns bs) >>= return . InfoTable

collectConditions :: UniqFM [PEAnnotation] -> [CoreBind] -> IM.IntMap [PEAnnotation]
collectConditions anns binds =
    IM.fromList $ mapMaybe (\b -> (coreBindKey b,) <$> annotated anns b) binds

-- collectInlines :: UniqFM [PEAnnotation] -> [CoreBind] -> IM.IntMap CoreBind
-- collectInlines anns bs = IM.fromList $ map (\b -> (coreBindKey b, b)) $ filter (annotated anns) bs

annotated :: UniqFM [PEAnnotation] -> CoreBind -> Maybe [PEAnnotation]
annotated anns b = lookupUFM anns (coreBindBinder b)

--------------------------------------------------------------------------------

congruence :: Eq a
           => (S.Set a -> [CoreBind] -> CoreM (S.Set a))
           -> S.Set a -> [CoreBind] -> S.Set a -> CoreM (S.Set a)
congruence f s0 b s1 = do
    if s1 == s0
      then return s0
      else do
        s2 <- f s1 b
        congruence f s1 b s2

reportStatics :: S.Set Name -> CoreBind -> CoreM (S.Set Name)
reportStatics s (NonRec b e) = do
    bStatic <- reportStatics_expr (S.insert (varName b) s) e
    if bStatic
      then reportStatic b >> return (S.insert (varName b) s)
      else liftIO (putStrLn (show b ++ " is not static.")) >> return s
reportStatics s (Rec bs)     = foldM f s bs
  where
    f :: S.Set Name -> (CoreBndr, CoreExpr) -> CoreM (S.Set Name)
    f s' (b, e) = reportStatics s' (NonRec b e)

knownVar :: Var -> Bool
knownVar v =
    let ret = or [ mod == "GHC.Num" && vName == "+"
                 , mod == "GHC.Num" && vName == "-"
                 , mod == "GHC.Num" && vName == "*"
                 , mod == "GHC.Types" && vName == "I#"
                 , mod == "GHC.Num" && vName == "$fNumInt" -- Num instance of Int
                 , mod == "GHC.Base" && vName == "$"

                 , mod == "GHC.Types" && vName == ":"
                 , mod == "GHC.Types" && vName == "[]"

                 , mod == "GHC.Prim" && vName == "void#"
                 , mod == "GHC.Prim" && vName == "Void#"

                 ]
     in if ret then ret else trace (show [mod, vName] ++ " is not known.") ret
  where
    name        = varName v
    occName     = nameOccName name
    mod         = moduleNameString $ moduleName $ nameModule name
    vName       = occNameString occName

reportStatics_expr :: S.Set Name -> CoreExpr -> CoreM Bool
reportStatics_expr s e
  | trace ("reportStatics_expr " ++ parens (show s) ++ " " ++ parens (show e)) False = undefined

reportStatics_expr s (Var v) = return $ S.member (varName v) s || knownVar v

reportStatics_expr _ Lit{} = return True
reportStatics_expr s e@(App f a) = do
    ret <- (&&) <$> reportStatics_expr s f <*> reportStatics_expr s a
    when ret $ reportStaticExpr e
    return ret
reportStatics_expr s (Lam arg body) =
    -- hm, I think right thing to do here is to add arg as static before checking
    -- body, because we'll mark this as static if arg in application site is
    -- static.
    reportStatics_expr (S.insert (varName arg) s) body
reportStatics_expr s (Let bs rhs) = do
    s' <- reportStatics s bs
    reportStatics_expr s' rhs
reportStatics_expr s (Case e b _ alts) = do
    eStatic <- reportStatics_expr s e
    if eStatic
      then and <$> mapM (reportStatics_alt $ S.insert (varName b) s) alts
      else return False
reportStatics_expr s (Cast e _) = reportStatics_expr s e
reportStatics_expr s (Tick _ e) = reportStatics_expr s e
reportStatics_expr _ Type{} = return True -- TODO: make sure this is correct
reportStatics_expr _ Coercion{} = return True

reportStatics_alt :: S.Set Name -> CoreAlt -> CoreM Bool
reportStatics_alt s (_, bs, e) = reportStatics_expr (foldl' (flip S.insert) s (map varName bs)) e

reportStatic :: Var -> CoreM ()
reportStatic v = liftIO $ putStrLn $ "Marking " ++ show v ++ " as static."

reportStaticExpr :: CoreExpr -> CoreM ()
reportStaticExpr e = liftIO $ putStrLn $ "Found static expr: " ++ show e

--------------------------------------------------------------------------------

printDecl :: IM.IntMap CoreBind -> CoreBind -> CoreM CoreBind
printDecl inlines b = do
    case IM.lookup (coreBindKey b) inlines of
      Nothing -> liftIO $ putStrLn "NOT INLINE"
      Just _  -> liftIO $ putStrLn "INLINE"
    -- liftIO $ putStrLn $ showOutputable b
    -- liftIO $ putStrLn $ show b
    -- liftIO $ putStrLn "after pass:"
    let b' = inlineApps inlines b
    liftIO $ putStrLn $ showOutputable b'
    liftIO $ putStrLn $ show b'
    return b'

inlineApps :: IM.IntMap CoreBind -> CoreBind -> CoreBind
inlineApps is (NonRec b e) = NonRec b (inlineApps_expr is e)
inlineApps is (Rec bs)     = Rec (map (second $ inlineApps_expr is) bs)

inlineApps_expr :: IM.IntMap CoreBind -> CoreExpr -> CoreExpr

inlineApps_expr is e@(App f arg) =
    let (f', args) = second (++ [arg]) $ collectArgs f
     in case f' of
          Var v ->
            case findReducer v of
              Nothing ->
                trace ("=== can't find reducer for " ++ showOutputable v) $
                  case lookupInline v is of
                    Nothing ->
                      trace ("=== also can't inline, not marked as inline")$
                        App (inlineApps_expr is f) (inlineApps_expr is arg)
                    Just bndr ->
                      inlineApps_expr is $ mkApp (coreBindBody bndr) (map (inlineApps_expr is) args)
              Just r  -> trace ("=== found a reducer for " ++ showOutputable v) $
                           case r is f' args of
                             Nothing -> trace "===== reduce failed" e
                             Just e' ->
                               trace ("===== reduce successful\nbefore: " ++ show e ++ "\nafter: " ++ show e') e'


            -- | Just bndr <- lookupInline v is ->
            --   let occName = nameOccName (varName v)
            --       nameStr = occNameString occName in
            --   trace ("=================== INLINING YEAAAHHH " ++ nameStr) $
            --     mkApp (coreBindBody bndr) (map (inlineApps_expr is) args)
            -- | otherwise ->
            --   let occName = nameOccName (varName v)
            --       nameStr = occNameString occName
            --       mod     = nameModule (varName v) in
            --   trace ("=================== NOT INLINING " ++ nameStr ++ " from " ++ showOutputable mod) $
            --     App (inlineApps_expr is f) (inlineApps_expr is arg)

          _ ->
              App (inlineApps_expr is f) (inlineApps_expr is arg)

inlineApps_expr _  e@Lit{} = e
inlineApps_expr _  e@Var{} = e
inlineApps_expr is (Lam arg body) = Lam arg (inlineApps_expr is body)
inlineApps_expr is (Let b e) = Let (inlineApps is b) (inlineApps_expr is e)
inlineApps_expr is (Case e b t alts) =
    Case (inlineApps_expr is e) b t (map (inlineApps_alt is) alts)
inlineApps_expr is (Cast e c) = Cast (inlineApps_expr is e) c
inlineApps_expr is (Tick t e) = Tick t (inlineApps_expr is e)
inlineApps_expr _  e@Type{} = e
inlineApps_expr _  e@Coercion{} = e

checkTCDicts :: CoreBind -> CoreM ()
checkTCDicts (NonRec _ e) = checkTCDicts_expr e
checkTCDicts (Rec bs)     = mapM_ (checkTCDicts_expr . snd) bs

checkTCDicts_expr :: CoreExpr -> CoreM ()
checkTCDicts_expr Lit{} = return ()
checkTCDicts_expr (Var v)
  | isId v
  , ClassOpId cls <- idDetails v
  = do liftIO (putStrLn $ "found ClassOpId of class " ++ showOutputable cls
                          ++ ": " ++ show (varName v))

  | isId v
  , DFunId{} <- idDetails v
  = do liftIO $ putStrLn $ "found DFunId: " ++ show (varName v)
       let unfolding = unfoldingInfo $ idInfo v
       liftIO $ putStrLn $ "unfolding info: " ++ showOutputable unfolding
       liftIO $ putStrLn $ "unfolding template: " ++ show (maybeUnfoldingTemplate unfolding)

  | otherwise = return ()

checkTCDicts_expr (App f a) = checkTCDicts_expr f >> checkTCDicts_expr a
checkTCDicts_expr (Lam _ body) = checkTCDicts_expr body
checkTCDicts_expr (Let b e) = checkTCDicts b >> checkTCDicts_expr e
checkTCDicts_expr (Case e _ _ alts) = checkTCDicts_expr e >> mapM_ checkTCDicts_alt alts
checkTCDicts_expr (Tick _ e) = checkTCDicts_expr e
checkTCDicts_expr (Cast e _) = checkTCDicts_expr e
checkTCDicts_expr Type{} = return ()
checkTCDicts_expr Coercion{} = return ()

checkTCDicts_alt :: CoreAlt -> CoreM ()
checkTCDicts_alt _ = return () -- FIXME

checkDictArgs :: [CoreExpr] -> String
checkDictArgs [] = ""
checkDictArgs (Var v : rest)
  | isTcClsNameSpace (occNameSpace (nameOccName (varName v)))
  = "found one,"
  | otherwise
  = checkDictArgs rest
checkDictArgs (_ : rest) = checkDictArgs rest

inlineApps_alt :: IM.IntMap CoreBind -> CoreAlt -> CoreAlt
inlineApps_alt _ alt = alt

mkApp :: Expr b -> [Arg b] -> Expr b
mkApp f [] = f
mkApp f (a : as) = mkApp (App f a) as

--------------------------------------------------------------------------------
-- * Reduction stuff

type Reducer = IM.IntMap CoreBind -> CoreExpr -> [CoreExpr] -> Maybe CoreExpr

findReducer :: Var -> Maybe Reducer
findReducer v = M.lookup (pk, modName, varName_) reducers
  where
    mod         = nameModule $ varName v
    pk          = packageKeyFS $ modulePackageKey mod
    modName     = moduleNameFS $ moduleName mod
    varName_    = occNameFS $ nameOccName $ varName v

reducers :: M.Map (FastString, FastString, FastString) Reducer
--                 (package key, module name)
-- with the module name, this hopefully makes reducers unique to functions.
-- (TODO: make sure package version number is included)
reducers = M.fromList $ map mkFSKey $
    [ -- ("base", "GHC.Num", "+", reducePlus)
      ("ghc-prim", "GHC.Prim", "+#", reducePlus)
    ]
  where
    mkFSKey :: (String, String, String, a) -> ((FastString, FastString, FastString), a)
    mkFSKey (packageKey, mod, var, r) =
      ((mkFastString packageKey, mkFastString mod, mkFastString var), r)

reducePlus :: Reducer

reducePlus im plus [arg1, arg2] =
    let arg1' = inlineApps_expr im arg1
        arg2' = inlineApps_expr im arg2
        (machInts, others) = splitInts [arg1', arg2']
        reducedInts =
          -- FIXME: If none of the args are reducable, this generates an extra
          -- (0 + ...)
          foldl' (+) 0 machInts in
    Just $ if null others
             then
               -- FIXME: Using hacky DynFlags here. Should be OK, but it's
               -- better to use non-hacky version.
               mkConApp intDataCon [mkIntLit dynFlags reducedInts]
             else mkApp plus (Lit (MachInt reducedInts) : others)
  where
    splitInts :: [CoreExpr] -> ([Integer], [CoreExpr])
    splitInts [] =
      ([], [])

    splitInts (Lit (MachInt i) : rest)
        -- TODO: remove redundancy
      -- |    modulePackageKey (nameModule (varName v)) == primPackageKey
      --   && moduleNameString (moduleName (nameModule (varName v))) == "GHC.Types"
      --   && occNameString (nameOccName (varName v)) == "I#"
      = first (i :) $ splitInts rest

    splitInts (notI : rest) =
      second (notI :) $ splitInts rest

reducePlus _  _     args = trace ("reducePlus: can't reduce args: " ++ showOutputable args) Nothing

-}

--------------------------------------------------------------------------------
-- * Utils

coreBindKey :: CoreBind -> Int
coreBindKey = getKey . getUnique . coreBindBinder

coreBindBinder :: CoreBind -> CoreBndr
coreBindBinder = head . bindersOf

-- WARNING: Only works for top-level binders, because assumes Rec list has only
-- one definition.
coreBindBody :: CoreBind -> CoreExpr
coreBindBody (NonRec _ e)   = e
coreBindBody (Rec [(_, e)]) = e
coreBindBody (Rec _)        = panic "coreBindBody: Empty list of bindings or more than one bindings."

lookupInline :: CoreBndr -> IM.IntMap CoreBind -> Maybe CoreBind
lookupInline b m = IM.lookup (getKey . getUnique $ b) m

parens :: String -> String
parens s = '(' : s ++ ")"
