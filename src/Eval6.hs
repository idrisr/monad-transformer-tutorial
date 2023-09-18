module Eval6 where

import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Writer
import qualified Data.Map as Map
import Eval

type Eval6 a = ReaderT Env (ExceptT String (WriterT [String] (StateT Integer IO))) a

runEval6 :: Env -> Integer -> Eval6 a -> IO ((Either String a, [String]), Integer)
runEval6 env st ev = runStateT (runWriterT (runExceptT (runReaderT ev env))) st

eval6 :: Exp -> Eval6 Value
eval6 (Lit i) = do
    tick
    liftIO $ print i
    return $ IntVal i
eval6 (Var n) = do
    tick
    tell [n]
    env <- ask
    case Map.lookup n env of
        Nothing -> throwError $ "unbound variable: " ++ n
        Just v -> return v
eval6 (Plus e f) = do
    e1 <- eval6 e
    f1 <- eval6 f
    case (e1, f1) of
        (IntVal i, IntVal j) -> return $ IntVal $ i + j
        _ -> throwError "type error in addition"
eval6 (Abs n e) = do
    tick
    env <- ask
    return $ FunVal env n e
eval6 (App e f) = do
    tick
    e1 <- eval6 e
    f1 <- eval6 f
    case e1 of
        FunVal env n body -> local (const (Map.insert n f1 env)) (eval6 body)
        _ -> throwError "type error in application"
