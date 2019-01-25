module Main where

import System.Console.ANSI
import Data.Number.CReal
import System.Console.Readline
import System.IO
import Text.Read

type Stack = [Rational]

main :: IO ()
main = run []

run :: Stack -> IO ()
run stack = do
  maybeLine <- readline "% "
  case maybeLine of
    Nothing -> pure ()
    Just ":q" -> pure ()
    Just "quit" -> pure ()
    Just input -> do
      addHistory input
      case input of
        "discard" -> discard stack
        "sqrt" -> sqrt' stack
        "e" -> const' stack $ exp 1
        "pi" -> const' stack pi
        c | c `elem` ["xy","swap"] -> xy stack
        c | c `elem` ["+","-","*","/","^","log"] ->
              doOperation stack input
        _ -> addNumber stack $ readMaybe input

doOperation :: Stack -> String -> IO ()
doOperation stack@(_:_:_) input = do
  let (result, stack') = perform stack input
  let stack'' = result:stack'
  -- move up 1 line for the enter you just pressed and 2 lines for the 2
  -- numbers we're popping
  upAndClear 3
  putStrLn $ printRational result
  run stack''
doOperation stack _ = do
  upAndClear 1
  run stack

addNumber :: Stack -> Maybe Double -> IO ()
addNumber stack Nothing = do
  upAndClear 1
  run stack
addNumber stack (Just input) = do
  let input' = toRational input
  let stack' = input':stack
  upAndClear 1
  print input
  run stack'

perform :: Stack -> String -> (Rational, Stack)
perform (y:x:s) op =
  case op of
    "+" -> (x + y,s)
    "-" -> (x - y,s)
    "*" -> (x * y,s)
    "/" -> (x / y,s)
    "log" ->
      let
        x' = realToFrac x :: Double
        y' = realToFrac y :: Double
        result = logBase y' x'
      in
        (toRational result,s)
    "^" ->
      let
        x' = realToFrac x :: Double
        y' = realToFrac y :: Double
        result = x' ** y'
      in
        (toRational result,s)
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
discardElement [] = []
discardElement (_:xs) = xs

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
sqrtStack [] = []
sqrtStack (x:xs) =
  toRational (sqrt (fromRational x :: Double)):xs
  

xy :: Stack -> IO ()
xy stack@(_:_:_) = do
  let stack' = xyStack stack
  upAndClear 3
  putStrLn $ printRational (stack' !! 1)
  putStrLn $ printRational (head stack')
  run stack'
xy xs = do
  upAndClear 1
  run xs

xyStack :: Stack -> Stack
xyStack (x:y:xs) = y:x:xs
xyStack xs = xs

const' :: Stack -> Double -> IO ()
const' xs x = do
  upAndClear 1
  let stack = constStack x xs
  putStrLn $ printRational (head stack)
  run stack

constStack :: Double -> Stack -> Stack
constStack x xs =
  x':xs
  where
    x' = toRational x


printRational :: Rational -> String
printRational rat = showCReal 13 $ fromRational rat

upAndClear :: Int -> IO ()
upAndClear n = do
  cursorUpLine n
  clearFromCursorToScreenEnd
