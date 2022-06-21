module Main where

import Debug.Trace
import Text.Read
import Pdf.Core.File hiding (withPdfFile)
import Pdf.Document
import System.Directory
import System.Console.ANSI

type Market = String
type Price = Maybe Double
type Commodity = String
data SuggestedRetailPrice = SuggestedRetailPrice (Commodity, Price)

data Config = Config
    {
        inputDir :: String,
        filePrefix :: String,
        fileExtension :: String
    }

-- This is where we keep track of processing states
data ParserState = ParserState 
    {
        currentPos :: Int,
        values :: [[Double]], -- list of values
        headers :: [[String]] -- list of headers
    }

-- break data into rows
-- break rows into fields
-- get the first field => Market
-- get succeeding fields => Retail Price
-- parse header row to get commodity names

commodities :: [String]
commodities = ["Well-milled Rice (Local)", "Tilapia", "Galunggong", "Whole Chicken", "Egg (Medium)", "Ampalaya", "Tomato", "Cabbage", "Pechay Baguio", "Red Onion (Local)" ]

-- extracts the market from the row
getMarket :: String -> String
getMarket = undefined

-- remove the word "AVAILABLE"
-- convert using readMaybe, NOT will become Nothing
getData :: String -> [Price]
getData = undefined

processMe :: Pdf -> IO ()
processMe pdf = do
    encrypted <- isEncrypted pdf
    print ("File is encrypted: " ++ show encrypted)
    doc <- document pdf
    catalog <- documentCatalog doc
    rootNode <- catalogPageNode catalog
    count <- pageNodeNKids rootNode
    print ("No. of pages: " ++ show count)
    page <- pageNodePageByNum rootNode 1
    fonts <- pageFontDicts page
    let fontNames = map (\x -> fst x) fonts
    glyphs <- pageExtractGlyphs page
    let text = glyphsToText glyphs
    print $ "End: " ++ (show fontNames)
    print $ text

printMenu :: IO ()
printMenu = do
    putStrLn ""
    putStrLn "**************"
    putStrLn "* Commands   *"
    putStrLn "**************"
    putStrLn "* 1 - List   *"
    putStrLn "* 2 - Search *"
    putStrLn "* 3 - Exit   *"
    putStrLn "**************"
    putStr "Enter choice:"

printHelper :: [(Int, String)] -> IO ()
printHelper [] = return ()
printHelper (x:xs) = do
    putStrLn $ show (fst x) ++ ") " ++ (snd x)
    printHelper xs

printFiles :: IO ()
printFiles = do
    files <- listDirectory "input"
    putStrLn ""
    putStrLn "[Files]"
    printHelper $ zip [1..] $ map getDateFromFile files

printSearch :: IO ()
printSearch = do
    putStrLn ""
    putStrLn "Enter file to search: "
    file <- getLine
    putStrLn "Enter commodity: "
    commodity <- getLine
    searchCommodity file commodity
    return ()

searchCommodity :: String -> String -> IO ()
searchCommodity dt cm = do
    let filepath = (getFilePath . getFileFromDate) dt
    exists <- doesFileExist filepath
    putStrLn $ "File exists: " ++ show exists
    putStrLn $ "Loading " ++ filepath
    putStrLn "Average price is..."
    putStrLn "Lowest price is... in ..."
    putStrLn "Highest price is... in ..."

-- removes the filename prefix and extension to get the date
getDateFromFile :: String -> String
getDateFromFile s =
    let l = length s - 4
        p = length "Price-Monitoring-"
    in drop p $ take l s

-- prepends the filename prefix and appends the extension to the date 
getFileFromDate :: String -> String
getFileFromDate s = "Price-Monitoring-" ++ s ++ ".pdf"

getFilePath :: String -> String
getFilePath s = "input/" ++ s

main :: IO ()
main = do
    printMenu
    choice <- getLine
    case choice of
        "1" -> do
            clearScreen
            printFiles
            main
        "2" -> do
            printSearch
            main
        "3" -> do
            putStrLn "\nTHAT'S ALL FOLKS"
            return ()
        otherwise -> main

--    withPdfFile "Price-Monitoring-June-10-2022.pdf" processMe
