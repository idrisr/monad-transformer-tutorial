module Main (main) where

import EvalTest
import Test.Tasty

tests :: TestTree
tests = testGroup "Main" [evalTests]

main :: IO ()
main = defaultMain tests
