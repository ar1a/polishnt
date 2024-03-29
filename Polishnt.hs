{-# LANGUAGE OverloadedStrings #-}

module Polishnt where

import           Control.Monad.IO.Class         ( liftIO )
import           Data.Number.CReal
import qualified Data.Text                     as T
import           System.Console.ANSI
import           System.Console.Haskeline
import           System.IO
import           Text.Read

type Stack = [Rational]

run :: Stack -> IO ()
run stack = runInputT defaultSettings loop
 where
  loop :: InputT IO ()
  loop = do
    maybeLine <- getInputLine "% "
    case maybeLine of
      Nothing     -> pure ()
      Just ":q"   -> pure ()
      Just "quit" -> pure ()
      Just input  -> liftIO $ do
        case input of
          "discard" -> discard stack
          "sqrt"    -> sqrt' stack
          "e"       -> const' stack $ exp 1
          "pi"      -> const' stack pi
          c
            | c `elem` ["xy", "swap"] -> xy stack
            | c `elem` ["+", "-", "*", "/", "^", "log"] -> doOperation stack
            $  T.pack input
          _ -> addNumber stack $ readInput $ T.pack input

readInput :: T.Text -> Maybe Double
readInput s | "." `T.isPrefixOf` s = readInput $ '0' `T.cons` s
            | otherwise            = readMaybe $ T.unpack s

doOperation :: Stack -> T.Text -> IO ()
doOperation stack@(_ : _ : _) input = do
  let stack' = perform stack input
  -- move up 1 line for the enter you just pressed and 2 lines for the 2
  -- numbers we're popping
  upAndClear 3
  putStrLn $ printRational (head stack')
  run stack'
doOperation stack _ = do
  upAndClear 1
  run stack

addNumber :: Stack -> Maybe Double -> IO ()
addNumber stack Nothing = do
  upAndClear 1
  run stack
addNumber stack (Just input) = do
  let stack' = addNumberStack stack input
  upAndClear 1
  print input
  run stack'

addNumberStack :: Stack -> Double -> Stack
addNumberStack stack n = toRational n : stack

perform :: Stack -> T.Text -> Stack
perform (y : x : s) op = case op of
  "+" -> x + y : s
  "-" -> x - y : s
  "*" -> x * y : s
  "/" -> x / y : s
  "log" ->
    let x'     = realToFrac x :: Double
        y'     = realToFrac y :: Double
        result = logBase y' x'
    in  toRational result : s
  "^" ->
    let x'     = realToFrac x :: Double
        y'     = realToFrac y :: Double
        result = x' ** y'
    in  toRational result : s
  _ -> error "you should never see this"
perform _ _ = error "you should never see this 2"

discard :: Stack -> IO ()
discard [] = do
  upAndClear 1
  run []
discard stack = do
  let stack' = discardElement stack
  upAndClear 2
  hFlush stdout
  run stack'

discardElement :: Stack -> Stack
discardElement []       = []
discardElement (_ : xs) = xs

sqrt' :: Stack -> IO ()
sqrt' [] = do
  upAndClear 1
  run []
sqrt' xs = do
  let stack = sqrtStack xs
  upAndClear 2
  putStrLn $ printRational (head stack)
  run stack

sqrtStack :: Stack -> Stack
sqrtStack []       = []
sqrtStack (x : xs) = toRational (sqrt (fromRational x :: Double)) : xs

xy :: Stack -> IO ()
xy stack@(_ : _ : _) = do
  let stack' = xyStack stack
  upAndClear 3
  putStrLn $ printRational (stack' !! 1)
  putStrLn $ printRational (head stack')
  run stack'
xy xs = do
  upAndClear 1
  run xs

xyStack :: Stack -> Stack
xyStack (x : y : xs) = y : x : xs
xyStack xs           = xs

const' :: Stack -> Double -> IO ()
const' xs x = do
  upAndClear 1
  let stack = addNumberStack xs x
  putStrLn $ printRational (head stack)
  run stack

printRational :: Rational -> String
printRational rat = showCReal 13 $ fromRational rat

upAndClear :: Int -> IO ()
upAndClear n = do
  cursorUpLine n
  clearFromCursorToScreenEnd
