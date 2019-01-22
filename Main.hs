module Main where

import System.Console.ANSI
import Data.Number.CReal
import System.Console.Readline
import System.IO
import Text.Read

main :: IO ()
main = do
  run []

run :: [Rational] -> IO ()
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
        "xy" -> xy stack
        c | c `elem` ["+","-","*","/","^","log"] ->
              doOperation stack input
        _ -> addNumber stack $ readMaybe input

doOperation :: [Rational] -> String -> IO ()
doOperation stack@(_:_:_) input = do
  let (result, stack') = perform stack input
  let stack'' = result:stack'
  -- move up 1 line for the enter you just pressed and 2 lines for the 2
  -- numbers we're popping
  cursorUpLine 3
  clearFromCursorToScreenEnd
  putStrLn $ printRational result
  run stack''
doOperation stack _ = do
  cursorUpLine 1
  clearLine
  run stack

addNumber :: [Rational] -> Maybe Double -> IO ()
addNumber stack Nothing = do
  cursorUpLine 1
  clearLine
  run stack
addNumber stack (Just input) = do
  let input' = toRational input
  let stack' = input':stack
  cursorUpLine 1
  clearLine
  print input
  run stack'

perform :: [Rational] -> [Char] -> (Rational, [Rational])
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

discard :: [Rational] -> IO ()
discard [] = do
  cursorUpLine 1
  clearLine
  run []
discard stack = do
  let _:stack' = stack
  cursorUpLine 2
  clearFromCursorToScreenEnd
  hFlush stdout
  run stack'

sqrt' :: [Rational] -> IO ()
sqrt' [] = do
  cursorUpLine 1
  clearLine
  run []
sqrt' (x:xs) = do
  let n = fromRational x :: Double
  let result = toRational $ sqrt n
  cursorUpLine 2
  clearFromCursorToScreenEnd
  putStrLn $ printRational result
  run $ result:xs

xy :: [Rational] -> IO ()
xy [] = do
  cursorUpLine 1
  clearLine
  run []
xy stack = do
  let x:y:stack' = stack
  let stack'' = y:x:stack'
  cursorUpLine 3
  clearFromCursorToScreenEnd
  putStrLn $ printRational x
  putStrLn $ printRational y
  run stack''

const' :: [Rational] -> Double -> IO ()
const' xs x = do
  cursorUpLine 1
  clearLine
  let x' = toRational x
  putStrLn $ printRational x'
  run $ x':xs


printRational :: Rational -> String
printRational rat = showCReal 13 $ fromRational rat
