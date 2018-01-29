{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

--import Data.Graph
module Syntax where

import qualified Data.Graph.Inductive.Graph as G
import Data.Graph.Inductive.PatriciaTree
import Data.Graph.Inductive.Query.DFS
import Data.Tree
import Data.List
import Data.Maybe
import qualified Control.Lens as L
import qualified Data.Map as Map

type Origin = Int
type ContextId = Int

data TypeId = TIBool | TIInt | TIChar | TIList | TIForAll | TITuple | TIFunction
                                                      deriving (Show, Eq)

data ParamInfo = ParamInfo { persName        :: Int
                           , originalContext :: ContextId
                           , currentContext  :: ContextId } deriving (Show, Eq)

data ValueType = 
      VFunction (Type, Type) -- function type
    | VTuple (Type, Type)    -- tuple type
    | VList Type          -- list type
    | VForallX Type       -- forall type
    | VInt                -- int type
    | VChar               -- char type
    | VBool               -- bool type
        deriving (Show)

getInnerType :: ValueType -> [Type]
getInnerType (VFunction (t1, t2)) = [t1, t2]
getInnerType (VTuple (t1, t2)) = [t1, t2]
getInnerType (VList t) = [t]
getInnerType (VForallX t) = [t]
getInnerType _ = []

data Type = 
      TyX ParamInfo        -- type variable
    | TyValue ValueType    -- value type    
    | Empty                -- empty type (substitute nothing)
            deriving (Show)

getParamInfo :: Type -> Maybe ParamInfo
getParamInfo (TyX info) = Just info
getParamInfo _ = Nothing

getValueType :: Type -> Maybe ValueType
getValueType (TyValue value) = Just value
getValueType _ = Nothing

data VarInfo = VarInfo { varName :: ParamInfo, constr :: Constraint } deriving (Show)

makeVarInfoByInfo :: ParamInfo -> VarInfo
makeVarInfoByInfo info = undefined

type OriginsList = [Origin]

data ValueConstraintData = SKind TypeId | DKind TypeId ValueConstraint | TKind TypeId (ValueConstraint, ValueConstraint)
                                                                                                  deriving (Show)
data ValueConstraint = ValueConstraint ValueConstraintData OriginsList [(ParamInfo, OriginsList)] deriving (Show)

data UnifiedConstraint = UnifiedConstraint { unifiedVariables :: ParamInfo
                                           , valueConstraints :: Map.Map ParamInfo ValueConstraint } 
                                                                              deriving (Show)
data Constraint = EmptyConstraint | VConstraint ValueConstraint | UConstraint UnifiedConstraint deriving (Show)

data Context = Context { number  :: Int
                       , contextData :: [VarInfo] } deriving (Show)

findContext :: [ Context ] -> ContextId -> Context
findContext scope n = fromJust $ find ((== n) . number) scope

addVarInfoToContext :: VarInfo -> Context -> Context
addVarInfoToContext info ctx = Context { number = (number ctx), 
                                         contextData = info : (contextData ctx) }

uncheckFindConstr :: Context -> ParamInfo -> Maybe VarInfo
uncheckFindConstr ctxt info = find byParamInfo (contextData ctxt)
                where 
                    byParamInfo x = (varName x) == info

findConstr :: Context -> ParamInfo -> VarInfo
findConstr c p = fromJust $ uncheckFindConstr c p

replaceConstr :: ParamInfo -> Constraint -> Context -> Maybe Context
replaceConstr n c ctxt = Just $ Context {number = number ctxt, contextData = replace' $ contextData ctxt}
    where 
        replace' [] = []
        replace' (x:xs) = current : replace' xs
            where current = if (varName x == n) then VarInfo {varName = n, constr = c} else x

data Node = Node { context :: Context, lambda :: Lambda }

type ComponentList = [(Int, Lambda)]

data SyntaxTree = SyntaxTree { compList :: ComponentList
                             , graph    :: Gr Lambda Origin
                             , sources  :: [ Int ]
                             , sinks    :: [ Int ] }

data Lambda = Lambda { name   :: String
                     , args   :: [ Type ]
                     , result ::  Type } deriving (Show)


typeId :: ValueType -> TypeId
--typeId (TyX _)                 = "TyX"
typeId (VFunction _) = TIFunction
typeId (VTuple _)    = TITuple
typeId (VList _)     = TIList
typeId (VForallX _)  = TIForAll
typeId VInt          = TIInt
typeId VChar         = TIChar
typeId VBool         = TIBool
--typeId Empty         = TIEmpty

isVar :: Type -> Bool
isVar (TyX _) = True
isVar _       = False

getVarName :: Type -> Maybe Int
getVarName (TyX info) = Just (persName info)
getVarName _       = Nothing 


funArg :: Type -> Lambda
funArg x = Lambda { name = "arg", args = [x], result = x }

{-inferNode :: Node -> Type -> Origin -> Maybe Node
inferNode node ty orig = 
                let context = inferer node
                    lam = lambda node
                    conn_point = (args lam) ^? element orig -}
                    

--inferTypes :: Node

--type SubTree = Tree Lambda

class Term a where
    inferType :: a -> [ Type ]

instance Term Lambda where
    inferType lam = [ result lam ]

instance Term SyntaxTree where
    inferType tree = undefined

{-instance Term SubTree where
    inferType stree = undefined-}

{-add_comp = Lambda { name   = "Add"
                  , args   = []--[ TyX 1, TyX 1 ]
                  , result = head (args add_comp) }

single_int = Lambda { name   = "Test Int"
                    , args   = [ TyInt ]
                    , result = head (args single_int) }

simple_graph :: Gr Lambda Origin
simple_graph = ([], 1, single_int, [(2, 2)]) &
                      (([], 2, add_comp, []) & empty)-}
