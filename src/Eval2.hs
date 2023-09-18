module Eval2 where

import Eval
import Control.Monad.Except
import Control.Monad.Identity
import qualified Data.Map as Map

type Eval2 a = ExceptT String Identity a

runEval2 :: Eval2 a -> Either String a
runEval2 ev = runIdentity (runExceptT ev)

eval2 :: Env -> Exp -> Eval2 Value
eval2 _ (Lit i) = return $ IntVal i
eval2 env (Var n) = case Map.lookup n env of
    Nothing -> throwError ("unbound variable: " ++ n)
    Just v -> return v
eval2 env (Plus e f) =
    do
        e1 <- eval2 env e
        f1 <- eval2 env f
        case (e1, f1) of
            (IntVal i1, IntVal i2) -> return $ IntVal (i1 + i2)
            _ -> throwError "type error in addition"
eval2 env (Abs n e) = return $ FunVal env n e
eval2 env (App e f) =
    do
        e1 <- eval2 env e
        f1 <- eval2 env f
        case e1 of
            FunVal env1 n body -> eval2 (Map.insert n f1 env1) body
            _ -> throwError "type error in application"
