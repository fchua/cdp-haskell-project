{-# LANGUAGE FlexibleContexts #-}

module Main where

import System.Console.ANSI
import System.Random
import System.IO
import Control.Monad.State
import Control.Monad.Reader
import Data.Maybe

type Point     = (Int, Int)
type Score     = Int
newtype Snake  = Snake { getSnake :: [Point] } deriving Show
newtype Food   = Food { getFood :: Point } deriving Show
data Direction = U | D | L | R deriving Show
data Game      = 
    Game {
        snake     :: Snake,
        food      :: Food,
        direction :: Direction,
        score     :: Score
    } deriving Show
type Size      = (Int, Int)
data Config    =
    Config {
        screenSize :: Size,
        headChar   :: Char,
        bodyChar   :: Char,
        foodChar   :: Char
    } deriving Show

initConfig :: MonadIO m => m Config
initConfig = do
    size <- liftIO getTerminalSize
    return $ Config {
        screenSize = fromJust size,
        headChar = 'o',
        bodyChar = 'x',
        foodChar = '@'
    }

initGame :: MonadIO m => m Game
initGame = do
    (rcol, rrow) <- randomIO
    config <- initConfig
    let (scol, srow) = screenSize config
    return $ Game {
        snake = Snake $ map ((,) 10) [ 12 .. 17 ] ,
        food = Food (mod rcol scol, mod rrow srow),
        direction = R,
        score = 0
    }

renderPoint :: Point -> Char -> IO ()
renderPoint (row, col) c = do
    setCursorPosition row col
    putChar c

renderGame :: ReaderT Config (StateT Game IO) ()
renderGame = do
    game <- liftIO $ initGame
    return ()

stateToStateT :: Monad m => State s a -> StateT s m a
stateToStateT sa = StateT $ \s -> return (runState sa s)

sa :: MonadState Int m => m ()
sa = modify (^ 2)

stio :: ReaderT Int (StateT Int IO) ()
stio = do
    v <- get
    liftIO $ putStrLn "hello, we are in `StateT in IO`!"
    liftIO $ print v
    sa
    w <- get
    liftIO $ print w

main :: IO ()
main = do
    runStateT (runReaderT stio 17) 7
    return ()
