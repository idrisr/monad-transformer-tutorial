module EvalTest where

import Test.Tasty
import Test.Tasty.HUnit
import Eval
import Eval0
import Eval1
import Eval2
import Eval3
import Eval4
import Eval5

import qualified Data.Map as Map

evalTests :: TestTree
evalTests =
    testGroup
        "Eval"
        [ eval0Tests
        , eval1Tests
        , eval2Tests
        , eval3Tests
        , eval4Tests
        , eval5Tests
        ]

exampleExp :: Exp
exampleExp = Lit 12 `Plus` App (Abs "x" (Var "x")) (Lit 4 `Plus` Lit 2)

exampleExp0 :: Exp
exampleExp0 = Lit 10 `Plus` App (Abs "x" (Var "x" `Plus` Var "y")) (Lit 4 `Plus` Lit 2)

exampleExp1 :: Exp
exampleExp1 = Lit 10 `Plus` Var "x"

exampleExp2 :: Exp
exampleExp2 = Lit 10 `Plus` Abs "x" (Var "x")

eval0Tests :: TestTree
eval0Tests =
    testGroup
        "Eval0"
        [ let env = Map.insert "y" (IntVal 53) Map.empty
              got = eval0 env exampleExp
              wot = IntVal 18
           in testCase "" $ got @?= wot
        , let env = Map.insert "y" (IntVal 53) Map.empty
           in testCase "" $ eval0 env exampleExp0 @?= IntVal 69
        , let env = Map.insert "x" (IntVal 53) Map.empty
           in testCase "" $ eval0 env exampleExp1 @?= IntVal 63
          -- , let env = Map.insert "x" (IntVal 53) Map.empty
          -- in testCase "" $ eval0 env exampleExp2 @?= IntVal 63
        ]

eval1Tests :: TestTree
eval1Tests =
    testGroup
        "Eval1"
        [ let env = Map.empty
              got = runEval1 $ eval1 env exampleExp
              wot = IntVal 18
           in testCase "" $ got @?= wot
        , let env = Map.insert "y" (IntVal 53) Map.empty
              got = runEval1 $ eval1 env exampleExp0
              wot = IntVal 69
           in testCase "" $ got @?= wot
        ]

eval2Tests :: TestTree
eval2Tests =
    let f x y = runEval2 $ eval2 x y
     in testGroup
            "Eval2"
            [ let env = Map.empty
                  got = f env exampleExp
                  wot = Right $ IntVal 18
               in testCase "" $ got @?= wot
            , let env = Map.insert "y" (IntVal 53) Map.empty
                  got = f env exampleExp0
                  wot = Right $ IntVal 69
               in testCase "" $ got @?= wot
            , let env = Map.empty
                  got = f env exampleExp2
                  wot = Left "type error in addition"
               in testCase "" $ got @?= wot
            ]

eval3Tests :: TestTree
eval3Tests =
    let f x y = runEval3 x $ eval3 y
     in testGroup
            "Eval3"
            [ let env = Map.empty
                  got = f env exampleExp
                  wot = Right $ IntVal 18
               in testCase "" $ got @?= wot
            , let env = Map.insert "y" (IntVal 53) Map.empty
                  got = f env exampleExp0
                  wot = Right $ IntVal 69
               in testCase "" $ got @?= wot
            , let env = Map.empty
                  got = f env exampleExp2
                  wot = Left "type error in addition"
               in testCase "" $ got @?= wot
            ]

eval4Tests :: TestTree
eval4Tests =
    let f x y = runEval4 x 0 $ eval4 y
     in testGroup
            "Eval4"
            [ let env = Map.empty
                  got = f env exampleExp
                  wot = (Right (IntVal 18), 8)
               in testCase "" $ got @?= wot
            , let env = Map.insert "y" (IntVal 53) Map.empty
                  got = f env exampleExp0
                  wot = (Right (IntVal 69), 10)
               in testCase "" $ got @?= wot
            , let env = Map.empty
                  got = f env exampleExp2
                  wot = (Left "type error in addition", 3)
               in testCase "" $ got @?= wot
            , let env = Map.empty
                  sut = Lit 420
                  got = f env sut
                  wot = (Right (IntVal 420), 1)
               in testCase "" $ got @?= wot
            , let env = Map.empty
                  sut = Lit 1 `Plus` Lit 68
                  got = f env sut
                  wot = (Right (IntVal 69), 3)
               in testCase "" $ got @?= wot
            ]

eval5Tests :: TestTree
eval5Tests =
    let f x y = runEval5 x 0 $ eval5 y
     in testGroup
            "Eval5"
            [ let env = Map.empty
                  got = f env exampleExp
                  wot = ((Right (IntVal 18), ["x"]), 8)
               in testCase "" $ got @?= wot
            , let env = Map.insert "y" (IntVal 53) Map.empty
                  got = f env exampleExp0
                  wot = ((Right (IntVal 69), ["x", "y"]), 10)
               in testCase "" $ got @?= wot
            , let env = Map.empty
                  got = f env exampleExp1
                  wot = ((Left "unbound variable: x", ["x"]), 3)
               in testCase "" $ got @?= wot
            , let env = Map.empty
                  got = f env exampleExp2
                  wot = ((Left "type error in addition", []), 3)
               in testCase "" $ got @?= wot
            , let env = Map.empty
                  sut = Lit 420
                  got = f env sut
                  wot = ((Right (IntVal 420), []), 1)
               in testCase "" $ got @?= wot
            , let env = Map.empty
                  sut = Lit 1 `Plus` Lit 68
                  got = f env sut
                  wot = ((Right (IntVal 69), []), 3)
               in testCase "" $ got @?= wot
            ]
