module Main where

main :: IO ()
main = do
    hSetBuffering stdin NoBuffering
    putStrLn "waiting for input..."
    hSelect [stdin] [] [] Nothing
    putStrLn "input ready"
    x <- getChar
    print x
