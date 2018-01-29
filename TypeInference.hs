module TypeInference where

import Syntax
import Helpers
import Data.Maybe
import Data.Tuple
import Data.Foldable
import Control.Arrow
import Control.Monad.State.Lazy
import Data.Functor.Identity
import Data.Graph.Inductive.Query.Monad

{- ****** Experiments with StateT ****** -}
type ContextState = StateT [ Context ] Maybe Context

--inferTypes :: (Type, Type) -> Maybe Context -> ContextState
--inferTypes (t1, t2) ctxt =  let new_cont = ctxt >>= infer (t1, t2)
--                                in --(new_cont, updateScope new_cont c_list)

--updateScope :: Maybe Context -> [ Context ] -> [ Context ]
--updateScope ctxt scope = case ctxt of 
--                                Nothing   -> scope
--                                Just cont -> replaceContext cont scope

--updateContexts :: Maybe Context -> [ Context ] -> Maybe (Context, [ Context ])
--updateContexts ctxt scope = case ctxt of 
--                                Nothing   -> Nothing
--                                Just cont -> replaceContext cont scope

--updateCtxts :: Maybe Context -> ContextState
--updateCtxts ctxt = StateT (st -> updateContexts ctxt st)

replaceContext :: Context -> [ Context ] -> [ Context ]
replaceContext ctxt (x:xs) = if (number ctxt) == (number x) 
                                    then ctxt:xs
                                    else x : (replaceContext ctxt xs)

putToTuple :: [ Context ] -> Context -> Maybe (Context, [ Context ])
putToTuple xs ctx = Just (ctx, xs)

transformSt :: (Context -> Maybe Context) -> Context -> StateT [ Context ] Maybe Context
transformSt f ctxt = let new_ctxt = f ctxt
                     in (StateT (\s -> new_ctxt >>= (putToTuple $ replaceContext ctxt s)))

{-  ****************************************************** -}

inferDispatch = uncurry ($) . swap

infer :: [ Context ] -> (Type, Type) -> Context -> Maybe Context

infer scope a@((TyX _), (TyX _)) ctxt = q >>= inferDispatch
    where
        t = processTypes (a, ctxt) (transferType scope)
        q = t >>= maybeTuple . (maybeTuple . (getParamInfo *** getParamInfo) *** Just . unify)
    
infer scope a@((TyX _), (TyValue _)) ctxt = q >>= inferDispatch
    where
        t = processTypes (a, ctxt) (transferType scope)
        q = t >>= maybeTuple . (maybeTuple . (getParamInfo *** getValueType) *** Just . setValue)
        
infer scope a@((TyValue _), (TyX _)) ctxt = infer scope (swap a) ctxt

infer scope a@((TyValue _), (TyValue _)) ctxt = q >>= inferDispatch
    where
        t = processTypes (a, ctxt) (transferType scope)
        q = t >>= maybeTuple . (maybeTuple . (getValueType *** getValueType) *** Just . (descent scope))

unify :: Context -> (ParamInfo, ParamInfo) -> Maybe Context
unify ctxt (n, m) = undefined

setValue :: Context -> (ParamInfo, ValueType) -> Maybe Context
setValue ctxt (n, t) = result
        where
            old_const = constr $ findConstr ctxt n
            value_const = makeValueConstraint $ TyValue t
            merged = addValueConstraint (old_const, value_const) ctxt
            result = merged >>= uncurry (replaceConstr n)
        

copyParameter :: [ Context ] -> ParamInfo -> Context -> Maybe (Type, Context)
copyParameter scope info ctxt = if (currentContext info) == (number ctxt) then Just (TyX info, ctxt)
                                                                          else Just (TyX info, addVarInfoToContext foundInfo ctxt)
                                                                              where
                                                                                origCtxt = findContext scope (originalContext info)
                                                                                foundInfo = findConstr origCtxt info

deepCopy :: [ Context ] -> (Type, Context) -> Maybe (Type, Context)
deepCopy scope ((TyX p), ctxt) = copyParameter scope p ctxt

deepCopy scope ((TyValue (VFunction t)), ctxt) = xx >>= Just . mapFst (TyValue . VFunction)
    where xx = processTypes (t, ctxt) (deepCopy scope)
    
deepCopy scope ((TyValue (VTuple t)), ctxt) = xx >>= Just . mapFst (TyValue . VTuple)
    where xx = processTypes (t, ctxt) (deepCopy scope)
    
deepCopy scope ((TyValue (VList t)), ctxt) = xx >>= Just . mapFst (TyValue . VList)
    where xx = processType (t, ctxt) (deepCopy scope)
    
deepCopy scope ((TyValue (VForallX t)), ctxt) = xx >>= Just . mapFst (TyValue . VForallX)
    where xx = processType (t, ctxt) (deepCopy scope)
    
deepCopy scope (t@(TyValue _), ctxt) = Just (t, ctxt)

descent :: [Context] -> Context -> (ValueType, ValueType) -> Maybe Context
descent scope ctxt (v1, v2) = if typeId v1 /= typeId v2
                        then Nothing
                        else foldrM (infer scope) ctxt $ zip (getInnerType v1) (getInnerType v2)

transferType :: [ Context ] -> (Type, Context) -> Maybe (Type, Context)
transferType scope (t@(TyX p), ctxt) = copyParameter scope p ctxt
transferType scope t@(TyValue _, _) = deepCopy scope t

makeValueConstraint :: Type -> ValueConstraint
makeValueConstraint (TyValue v@VInt) = ValueConstraint (SKind $ typeId v) [0] []
makeValueConstraint (TyValue v@VChar) = ValueConstraint (SKind $ typeId v) [0] []
makeValueConstraint (TyValue v@VBool) = ValueConstraint (SKind $ typeId v) [0] []
makeValueConstraint (TyValue v@(VList i)) = ValueConstraint (DKind (typeId v) (makeValueConstraint i)) [0] []
makeValueConstraint (TyValue v@(VForallX i)) = ValueConstraint (DKind (typeId v) (makeValueConstraint i)) [0] []
makeValueConstraint (TyValue v@(VFunction (l, r))) = ValueConstraint (TKind (typeId v) ((makeValueConstraint l), (makeValueConstraint r))) [0] []
makeValueConstraint (TyValue v@(VTuple (l, r))) = ValueConstraint (TKind (typeId v) ((makeValueConstraint l), (makeValueConstraint r))) [0] []

addValueConstraint :: (Constraint, ValueConstraint) -> Context -> Maybe (Constraint, Context)
addValueConstraint (EmptyConstraint, vc) ctxt = Just (VConstraint vc, ctxt)
addValueConstraint (VConstraint v, vc) ctxt = addValueToValueConstraint (v, vc) ctxt >>= Just . first VConstraint
addValueConstraint (UConstraint u, vc) ctxt = addValueToUnifiedConstraint (u, vc) ctxt >>= Just . first UConstraint

addValueToUnifiedConstraint :: (UnifiedConstraint, ValueConstraint) -> Context -> Maybe (UnifiedConstraint, Context)
addValueToUnifiedConstraint = undefined

addValueToValueConstraint :: (ValueConstraint, ValueConstraint) -> Context -> Maybe (ValueConstraint, Context)
addValueToValueConstraint (ValueConstraint ld lo lp, ValueConstraint rd ro rp) ctxt = result
    where
        check' :: ValueConstraintData -> ValueConstraintData -> Maybe (ValueConstraintData, Context)
        check' (SKind lid) (SKind rid) = if lid == rid then Just (SKind lid, ctxt) else Nothing
        check' (DKind lid li) (DKind rid ri) = if lid == rid
            then processType ((li, ri), ctxt) (uncurry addValueToValueConstraint) >>= Just . first (DKind lid)
            else Nothing
        check' (TKind lid (ll, lr)) (TKind rid (rl, rr)) = if lid == rid
            then processTypes (((ll, rl), (lr, rr)), ctxt) (uncurry addValueToValueConstraint) >>= Just . first (TKind lid)
            else Nothing
        check' _ _ = Nothing
        result = check' ld rd >>= Just . first (\x -> ValueConstraint x (lo ++ ro) (lp ++ rp))

