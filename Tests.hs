import TypeInference
import Syntax

var = ParamInfo { persName = 1, originalContext = 1, currentContext = 1 }

emptyContext :: Maybe Context
emptyContext = let var_info = VarInfo { varName = var, constr = EmptyConstraint } 
        in Just Context { number = 1, contextData = [ var_info] }

test2 :: Maybe Context
test2 = emptyContext >>= infer ((TyX var), (TyValue VInt))

test3 :: Maybe Context
test3 = test2 >>= infer ((TyX var), (TyValue VInt))


test4 :: Maybe Context
test4 = test3 >>= infer ((TyX var), (TyValue VBool))

testList1 :: Maybe Context
testList1 = emptyContext >>= infer ((TyX var), TyValue (VList (TyValue VInt)))

testList2 :: Maybe Context
testList2 = testList1 >>= infer (TyX var, TyValue VInt)

testList3 :: Maybe Context
testList3 = testList1 >>= infer (TyX var, TyValue (VFunction ((TyValue VBool), (TyValue VInt))))

testList4 :: Maybe Context
testList4 = testList1 >>= infer (TyX var, TyValue (VList (TyValue VChar)))
