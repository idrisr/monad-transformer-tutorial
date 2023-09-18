module Eval where

import qualified Data.Map as Map
import Control.Monad.State

type Name = String
type Env = Map.Map Name Value

data Exp
    = Lit Integer
    | Var Name
    | Plus Exp Exp
    | Abs Name Exp
    | App Exp Exp
    deriving (Show, Eq)

data Value
    = IntVal Integer
    | FunVal Env Name Exp
    deriving (Show, Eq)

tick :: (Num s, MonadState s m) => m ()
tick = do
    st <- get
    put $ st + 1
