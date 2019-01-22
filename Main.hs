module Main where

import Control.Concurrent
import System.Console.ANSI
import System.Console.Readline
import System.IO
import Text.Read

main :: IO ()
main = do
  run []

run :: [Double] -> IO ()
run stack = do
  maybeLine <- readline "% "
  case maybeLine of
    Nothing -> pure ()
    Just ":q" -> pure ()
    Just "quit" -> pure ()
    Just input -> do
      addHistory input
      case input of
        c
          | c `elem` ["+","-","*","/","^"] ->
            doOperation stack input
          | c == "discard" ->
          discard stack
          | c == "xy" ->
          xy stack
          | otherwise -> addNumber stack $ readMaybe c

doOperation :: [Double] -> String -> IO ()
doOperation stack input
  | length stack < 2 = do
    cursorUpLine 1
    clearLine
    run stack
  | otherwise = do
    let (result, stack') = perform stack input
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
    (y, stack') = pop stack
    (x, s) = pop stack'
  in
    case op of
      "+" -> (x + y,s)
      "-" -> (x - y,s)
      "*" -> (x * y,s)
      "/" -> (x / y,s)
      "^" -> (x ** y,s)
      _ -> error "you should never see this"

discard :: [Double] -> IO ()
discard stack
  | stack == [] = do
    cursorUpLine 1
    clearLine
    run stack
  | otherwise = do
    let (_, stack') = pop stack
    cursorUpLine 2
    clearFromCursorToScreenEnd
    hFlush stdout
    run stack'

xy :: [Double] -> IO ()
xy stack
  | stack == [] = do
    cursorUpLine 1
    clearLine
    run stack
  | otherwise = do
      let (x, stack') = pop stack
      let (y, stack'') = pop stack'
      let stack''' = push x stack''
      let stack'''' = push y stack'''
      cursorUpLine 3
      clearFromCursorToScreenEnd
      putStrLn $ show x
      putStrLn $ show y
      run stack''''
-----------------------------------
push :: a -> [a] -> [a]
push x xs = x:xs

pop :: [a] -> (a, [a])
pop [] = error "stack empty"
pop (x:xs) = (x, xs)
