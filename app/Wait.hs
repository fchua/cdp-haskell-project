module Main where

import System.IO

main :: IO ()
main = do
    hSetBuffering stdin NoBuffering
    putStrLn "waiting 3 seconds for input..."
    result <- hWaitForInput stdin 3000
    if result then
        getLine >>= \input -> putStrLn $ "you have entered: " ++ input
    else
        putStrLn "timeout!"
