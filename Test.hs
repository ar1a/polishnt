{-# LANGUAGE TemplateHaskell #-}
module Tests where

import Test.QuickCheck
-- import Test.QuickCheck.Modifiers (NonEmptyList (..))
import Polishnt

prop_add_length :: Double -> Stack -> Bool
prop_add_length n xs =
  length (addNumberStack xs n) == length xs + 1

prop_added_to_stack :: Double -> Stack -> Bool
prop_added_to_stack n xs =
  head (addNumberStack xs n) == toRational n

prop_discard :: Stack -> Property
prop_discard stack@(_:xs) = property $ discardElement stack == xs
prop_discard _ = property Discard

prop_xy :: Stack -> Property
prop_xy stack@(x:y:xs) = property $ xyStack stack == y:x:xs
prop_xy _ = property Discard

prop_add :: Double -> Double -> Bool
prop_add = test_binary "+" (+)

prop_div :: Double -> Positive Double -> Bool
prop_div a (Positive b) = test_binary "/" (/) a b

prop_mul :: Double -> Double -> Bool
prop_mul = test_binary "*" (*)

prop_sub :: Double -> Double -> Bool
prop_sub = test_binary "-" (-)

test_binary
  :: String
  -> (Rational -> Rational -> Rational)
  -> Double
  -> Double
  -> Bool
test_binary op op' a b =
  head (perform(addNumberStack (addNumberStack [] a) b) op) == toRational (op' a' b')
  where
    a' = toRational a
    b' = toRational b

return []
runTests :: IO Bool
runTests = $forAllProperties $ quickCheckWithResult (stdArgs {maxSuccess = 10000})

