module Main where

import Control.Monad.State
import Debug.Trace

-- Example use of State monad
-- Passes a string of dictionary {a,b,c}
-- Game is to produce a number from the string.
-- By default the game is off, a C toggles the
-- game on and off. A 'a' gives +1 and a b gives -1.
-- E.g 
-- 'ab'    = 0
-- 'ca'    = 1
-- 'cabca' = 0
-- State = game is on or off & current score
--       = (Bool, Int)

type ParserValue = String
type ParserState = (Bool, String)

parseMarket :: String -> State ParserState ParserValue
parseMarket [] = do
    (_, val) <- get
    return val

parseMarket (x:xs) = do
    (on, val) <- get
    case (trace ("x is " ++ show x) x) of
         '\n' | on -> put (False, val)
         _ | on    -> put (on, val ++ [x])
         _         -> put (on, val)
    parseMarket xs

startState = (True, "")

main = print $ evalState (parseMarket "Pritil Market/Manila 43.00 120.00 200.00 200.00 6.00 120.00 62.50 60.00 70.00 80.00\n") startState
