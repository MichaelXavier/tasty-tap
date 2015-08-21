module Main
    ( main
    ) where


-------------------------------------------------------------------------------
import           Test.Tasty
import           Test.Tasty.HUnit
import           Test.Tasty.Runners.TAP
-------------------------------------------------------------------------------

--TODO: actually make assertions
main :: IO ()
main = defaultMainWithIngredients [tapRunner] exampleTests


-------------------------------------------------------------------------------
exampleTests :: TestTree
exampleTests = testGroup "some example tests"
  [
    testCase "passes" $ assertBool "uh your computer is busted" True
  , testCase "fails" $ assertFailure "this was doomed"
  ]
