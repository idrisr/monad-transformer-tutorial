module Eval0 where

import qualified Data.Map as Map
import Data.Maybe
import Eval

eval0 :: Env -> Exp -> Value
eval0 _ (Lit i) = IntVal i
eval0 env (Var n) = fromJust $ Map.lookup n env
eval0 env (Plus e1 e2) = IntVal $ i1 + i2
  where
    IntVal i1 = eval0 env e1
    IntVal i2 = eval0 env e2
eval0 env (Abs n e) = FunVal env n e
eval0 env (App e1 e2) =
    let val1 = eval0 env e1
        val2 = eval0 env e2
     in case val1 of
            FunVal env1 n body -> eval0 (Map.insert n val2 env1) body
