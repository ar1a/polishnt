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
  let _:stack' = stack
  upAndClear 2
  hFlush stdout
  run stack'

sqrt' :: Stack -> IO ()
sqrt' [] = do
  upAndClear 1
  run []
sqrt' (x:xs) = do
  let n = fromRational x :: Double
  let result = toRational $ sqrt n
  upAndClear 2
  putStrLn $ printRational result
  run $ result:xs

xy :: Stack -> IO ()
xy [] = do
  upAndClear 1
  run []
xy stack = do
  let x:y:stack' = stack
  let stack'' = y:x:stack'
  upAndClear 3
  putStrLn $ printRational x
  putStrLn $ printRational y
  run stack''

const' :: Stack -> Double -> IO ()
const' xs x = do
  upAndClear 1
  let x' = toRational x
  putStrLn $ printRational x'
  run $ x':xs


printRational :: Rational -> String
printRational rat = showCReal 13 $ fromRational rat

upAndClear :: Int -> IO ()
upAndClear n = do
  cursorUpLine n
  clearFromCursorToScreenEnd
