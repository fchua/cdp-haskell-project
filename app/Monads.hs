--import Data.Monoid
import Control.Monad.Writer

--type Food = String
--type Price = Sum Int

--addDrink :: Food -> (Food,Price)
--addDrink "beans" = ("milk", Sum 25)
--addDrink "jerky" = ("whiskey", Sum 99)
--addDrink _ = ("beer", Sum 30)

--applyLog :: (Monoid m) => (a, m) -> (a -> (b,m)) -> (b,m)
--applyLog (x, log) f = 
--    let (y, newLog) = f x in (y, log `mappend` newLog)

logNumber :: Int -> Writer [String] Int
logNumber x = writer (x, ["Got number: " ++ show x])

logNumber2 :: Int -> Writer [String] Int  
logNumber2 x = do
    tell ["Got number: " ++ show x]
    return x

multWithLog :: Writer [String] Int
multWithLog = do
    a <- logNumber2 (3)
    b <- logNumber2 (5)
    c <- logNumber2 (100)
    return (a * b * c)

main :: IO ()
main = print $ runWriter multWithLog
