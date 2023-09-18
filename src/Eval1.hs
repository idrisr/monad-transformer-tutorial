module Eval1 where

import Eval
import qualified Data.Map as Map
import Control.Monad.Identity

type Eval1 a = Identity a

runEval1 :: Eval1 a -> a
runEval1 = runIdentity

eval1 :: Env -> Exp -> Eval1 Value
eval1 env (Lit i) = return $ IntVal i
eval1 env (Var n) = case Map.lookup n env of
    Nothing -> error ("unbound variable: " ++ n)
    Just v -> return v
eval1 env (Plus e1 e2) =
    do
        ~(IntVal i1) <- eval1 env e1
        ~(IntVal i2) <- eval1 env e2
        return $ IntVal (i1 + i2)
eval1 env (Abs n e) = return $ FunVal env n e
eval1 env (App e1 e2) =
    do
        v1 <- eval1 env e1
        v2 <- eval1 env e2
        case v1 of
            FunVal env1 n body -> eval1 (Map.insert n v2 env1) body
