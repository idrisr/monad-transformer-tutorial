module Eval4 where

import Control.Monad.Except
import Control.Monad.Identity
import Control.Monad.Reader
import Control.Monad.State
import qualified Data.Map as Map
import Eval

type Eval4 a = ReaderT Env (ExceptT String (StateT Integer Identity)) a

runEval4 :: Env -> Integer -> Eval4 a -> (Either String a, Integer)
runEval4 env st ev = runIdentity $ runStateT (runExceptT (runReaderT ev env)) st

eval4 :: Exp -> Eval4 Value
eval4 (Lit i) = do
    tick
    return $ IntVal i
eval4 (Var n) = do
    tick
    env <- ask
    case Map.lookup n env of
        Nothing -> throwError $ "unbound variable: " ++ n
        Just v -> return v
eval4 (Plus e f) = do
    tick
    e1 <- eval4 e
    f1 <- eval4 f
    case (e1, f1) of
        (IntVal i, IntVal j) ->
            return $ IntVal $ i + j
        _ -> throwError "type error in addition"
eval4 (Abs n e) = do
    tick
    env <- ask
    return $ FunVal env n e
eval4 (App e f) = do
    tick
    e1 <- eval4 e
    f1 <- eval4 f
    case e1 of
        FunVal env n body -> local (const (Map.insert n f1 env)) (eval4 body)
        _ -> throwError "type error in application"
