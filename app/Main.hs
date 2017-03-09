module Main where

import SimpleVM.VM
import SimpleVM.Asm
import System.IO
import System.Environment (getArgs)

printVm :: VmState -> IO ()
printVm vm = do
    putStrLn $ unlines $ vmTrace vm
    putStrLn "======================"
    putStrLn $ unlines $ vmOutput vm

run :: String -> IO ()
run contents = do
    case compile contents of
        Left err -> print err
        Right prog -> printVm $ runSimpleVm $ generate prog

runWithFilename :: String -> IO ()
runWithFilename fileName = do
    contents <- readFile fileName
    run contents

main :: IO ()
main = do
  args <- getArgs
  case args of
    []     -> putStrLn "Please provide a file name."
    (fn:_) -> runWithFilename fn
