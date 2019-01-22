module Main where

import System.Console.ANSI
import System.Console.Readline
import Text.Read
import Control.Concurrent
import System.IO

main :: IO ()
main = do
  run []

pause :: IO ()
pause = do
  hFlush stdout
  threadDelay 1000000

run :: [Double] -> IO ()
run stack = do
  maybeLine <- readline "% "
  case maybeLine of
    Nothing -> pure ()
    Just ":q" -> pure ()
    Just "quit" -> pure ()
    Just input -> do
      addHistory input
      if (input `elem` ["+","-","*","/","^"]) then
        doOperation stack input
        else 
        addNumber stack $ readMaybe input

doOperation :: [Double] -> String -> IO ()
doOperation stack input
  | length stack < 2 = do
    cursorUpLine 1
    clearLine
    run stack
  | otherwise = do
    let output = perform stack input
    let result = fst output
    let stack' = snd output
    let stack'' = push result stack'
    -- move up 1 line for the enter you just pressed and 2 lines for the 2
    -- numbers we're popping
    cursorUpLine 3
    clearFromCursorToScreenEnd
    putStrLn $ show result
    run stack''

addNumber :: [Double] -> Maybe Double -> IO ()
addNumber stack Nothing = do
  cursorUpLine 1
  clearLine
  run stack
addNumber stack (Just input) = do
  let stack' = push input stack
  cursorUpLine 1
  clearLine
  putStrLn $ show input
  run stack'

perform :: [Double] -> String -> (Double, [Double])
perform stack op =
  let
    y' = pop stack
    y = fst y'
    stack' = snd y'
    x' = pop stack'
    x = fst x'
    s = snd x'
  in
    case op of
      "+" -> (x + y,s)
      "-" -> (x - y,s)
      "*" -> (x * y,s)
      "/" -> (x / y,s)
      "^" -> (x ** y,s)
      _ -> error "you should never see this"


-----------------------------------
push :: a -> [a] -> [a]
push x xs = x:xs

pop :: [a] -> (a, [a])
pop [] = error "stack empty"
pop (x:xs) = (x, xs)
