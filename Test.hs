module Tests where

import Test.QuickCheck
-- import Test.QuickCheck.Modifiers (NonEmptyList (..))
import Polishnt

runTests :: IO ()
runTests = do
  quickCheck (withMaxSuccess 10000 prop_add_length)
  quickCheck (withMaxSuccess 10000 prop_added_to_stack)
  quickCheck (withMaxSuccess 10000 prop_add_inv)
  quickCheck (withMaxSuccess 10000 prop_xy_inv)


prop_add_length :: Double -> Bool
prop_add_length n =
  length (addNumberStack [] n) == 1

prop_added_to_stack :: Double -> Bool
prop_added_to_stack n =
  head (addNumberStack [] n) == toRational n

prop_add_inv :: Double -> Stack -> Bool
prop_add_inv n xs =
  discardElement (addNumberStack xs n) == xs

prop_xy_inv :: Stack -> Bool
prop_xy_inv xs =
  xyStack (xyStack xs) == xs

