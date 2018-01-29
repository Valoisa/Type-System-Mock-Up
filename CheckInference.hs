module CheckInference where

import Syntax
import Helpers
import Data.Maybe
import Data.Tuple
import Data.List
import Data.Foldable
import Data.Map.Strict
import Control.Arrow
import Control.Monad.State.Lazy
import Data.Functor.Identity
import Data.Graph.Inductive.Query.Monad

{-checkConstraint :: [ Context ] -> (Constraint, Constraint) -> Context -> Maybe Context
checkConstraint scope (EmptyConstraint, _) ctxt = Just ctxt
checkConstraint scope (VConstraint (ValueConstraint ldata lorig lparlist), VConstraint (ValueConstraint rdata rorig rparlist)) ctxt = 
    let leftnames  = map fst lparlist
        rightnames = map fst rparlist
        common     = intersect
checkConstraint scope (UConstraint luconstr, UConstraint ruconstr) ctxt = 
    let leftconstrs  = map elems (valueConstraints luconstr)
        rightconstrs = map elems (valueConstraints ruconstr)
    in -}

checkConstraintData :: ValueConstraintData -> ValueConstraintData -> Bool
checkConstraintData (SKind lid) (SKind rid) = lid == rid
checkConstraintData (DKind lid lcons) (DKind rid rcons) = lid == rid
checkConstraintData (TKind lid (lvc1, lvc2)) (TKind rid (rvc1, rvc2)) = lid == rid 
--            then firstCheck >>= secondCheck
--                where 
--                    firstCheck  = checkConstraint scope (VConstraint lvc1, VConstraint rvc1) ctxt
--                    secondCheck = checkConstraint scope (VConstraint lvc2, VConstraint rvc2)
checkConstraintData _ _  = False
