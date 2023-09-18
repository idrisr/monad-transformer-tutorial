module Eval5 where

import Control.Monad.Except
import Control.Monad.Identity
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Writer
import qualified Data.Map as Map
import Eval

type Eval5 a = ReaderT Env (ExceptT String (WriterT [String] (StateT Integer Identity))) a

runEval5 :: Env -> Integer -> Eval5 a -> ((Either String a, [String]), Integer)
runEval5 env st ev = runIdentity $ runStateT (runWriterT (runExceptT (runReaderT ev env))) st

eval5 :: Exp -> Eval5 Value
eval5 (Lit i) = do
    tick
    return $ IntVal i
eval5 (Var n) = do
    tick
    tell [n]
    env <- ask
    case Map.lookup n env of
        Nothing -> throwError $ "unbound variable: " ++ n
        Just v -> return v
eval5 (Plus e f) = do
    tick
    e1 <- eval5 e
    f1 <- eval5 f
    case (e1, f1) of
        (IntVal i, IntVal j) -> return $ IntVal $ i + j
        _ -> throwError "type error in addition"
eval5 (Abs n e) = do
    tick
    env <- ask
    return $ FunVal env n e
eval5 (App e f) = do
    tick
    e1 <- eval5 e
    f1 <- eval5 f
    case e1 of
        FunVal env n body -> local (const (Map.insert n f1 env)) (eval5 body)
        _ -> throwError "type error in application"
