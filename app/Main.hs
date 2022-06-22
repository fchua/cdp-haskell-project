module Main where

import Debug.Trace
import Text.Read
import Pdf.Core.File hiding (withPdfFile)
import Pdf.Document
import System.Directory
import System.Console.ANSI
import qualified Data.Text as T

type Market = String
type Price = Maybe Double
type Commodity = String
type CommodityPrice = (Commodity, Price)

-- not used, just an attempt to use StateMonad
data Config = Config
    {
        inputDir :: String,
        filePrefix :: String,
        fileExtension :: String
    }

type PriceRow = [ Price ]

data PriceTable = PriceTable 
    {
        headers :: [ Commodity ],
        rows :: [ PriceRow ]
    }

-- break data into rows
-- break rows into fields
-- get the first field => Market
-- get succeeding fields => Retail Price
-- parse header row to get commodity names

dir :: String
dir = "input"

prefix :: String
prefix = "Price-Monitoring-"

months :: [ String ]
months = [
    "January", 
    "February", 
    "March", 
    "April", 
    "May", 
    "June", 
    "July", 
    "August", 
    "September", 
    "October", 
    "November", 
    "December" ]

commodities :: [ String ]
commodities = [ 
    "Well_Milled_Rice", 
    "Tilapia", 
    "Galunggong", 
    "Whole_Chicken", 
    "Egg_Medium", 
    "Ampalaya", 
    "Tomato", 
    "Cabbage", 
    "Pechay_Baguio", 
    "Red_Onion" ]

searchAndDisplayResults :: String -> Pdf -> IO ()
searchAndDisplayResults c pdf = do
    doc <- document pdf
    catalog <- documentCatalog doc
    rootNode <- catalogPageNode catalog
    page <- pageNodePageByNum rootNode 1
    text <- pageExtractText page
    let noFooter = removeFooter text
    let noExtraLines = T.unpack $ cleanData noFooter
--    putStrLn noExtraLines
    let stringLines = lines noExtraLines
--    putStrLn $ show stringLines
    let (h, d) = splitDataToHeaderAndData stringLines
    putStrLn $ show h
    putStrLn $ take 200 $ repeat '*'
    let cleanedData = processDataRows h d
    printTable cleanedData
    putStrLn $ take 200 $ repeat '*'
    filterAndPrintTable c cleanedData

-- discard footer text
removeFooter :: T.Text -> T.Text
removeFooter text = fst $ T.breakOn (T.pack "Source:") text

-- list of string replacements
replaceList :: [ (String, String) ]
replaceList = [ 
    ("\nNOT \nAVAILABLE\n"," - "), 
    ("\nWell-milled \nRice (Local)", "Well_Milled_Rice"), 
    ("\nTilapia", " Tilapia"), 
    ("\nWhole \nChicken"," Whole_Chicken"), 
    ("\n*Egg \n(Medium)", " Egg_Medium"), 
    ("\nAmpalaya", " Ampalaya"), 
    ("\nPechay \nBaguio", " Pechay_Baguio "),
    ("\nRed Onion \n(Local)", "Red_Onion") ]

-- converts pair of String to pair of Text
convertToText :: [ (String, String) ] -> [ (T.Text, T.Text) ]
convertToText xs = map (\x -> (T.pack $ fst x, T.pack $ snd x) ) xs

-- cleans the raw data so it can be properly parsed
cleanData :: T.Text -> T.Text
cleanData = searchAndReplaceAll $ convertToText $ replaceList

-- cleans the raw headers
searchAndReplaceAll :: [(T.Text, T.Text)] -> T.Text -> T.Text
--searchAndReplaceAll [x] text = T.replace (T.pack $ fst x) (T.pack $ snd x) text
--searchAndReplaceAll (x:xs) text = T.replace (T.pack $ fst x) (T.pack $ snd x) (searchAndReplaceAll xs text)
searchAndReplaceAll xs text = foldr (\x -> T.replace (fst x) (snd x)) text xs

-- split headers and markets
splitDataToHeaderAndData :: [String] -> ([String], [String])
splitDataToHeaderAndData xs = 
    let (h, d) = splitAt 1 $ xs
    in (words $ head h, d)

splitDataToHeaderAndData' :: T.Text -> ([T.Text], [T.Text])
splitDataToHeaderAndData' text = 
    let (h, d) = splitAt 1 $ T.lines text
    in (T.words $ head h, d)    

-- split market and data
-- New Las Piñas City Public Market 40.00 140.00 240.00 200.00 6.00 140.00 100.00 80.00 80.00 100.00
-- ([Text],[Text])
splitDataToMarketAndData' :: Int -> String -> ([String], [String])
splitDataToMarketAndData' num text = 
    let tokens = words text
    in splitAt (length tokens - num) tokens

-- convert to proper type
-- (Market, [Price])
-- ("New Las Piñas City Public Market", [Maybe 40.00, Maybe 140.00, Maybe 240.00, ...])
splitDataToMarketAndData :: [String] -> String -> (Market, [CommodityPrice])
splitDataToMarketAndData headers text = 
    let (m, d) = splitDataToMarketAndData' (length headers) text
    in (unwords m, zip headers $ map (\x -> readMaybe $ x) d)

processDataRows :: [String] -> [String] -> [(Market, [CommodityPrice])]
processDataRows headers rows = map (\x -> splitDataToMarketAndData headers x) rows

filterAndPrintTable :: String -> [(Market, [CommodityPrice])] -> IO ()
filterAndPrintTable s xs = 
    let ys = map (\x -> (fst x, filterCommodity s $ snd x) ) xs
    in printTable ys

filterCommodity :: String -> [CommodityPrice] -> [CommodityPrice]
filterCommodity s xs = filter (\x -> (fst x) == s) xs

printTable :: [(Market, [CommodityPrice])] -> IO ()
printTable xs = do
    mapM printMarket xs
    return ()

printMarket :: (Market, [CommodityPrice]) -> IO ()
printMarket p = do
    putStrLn $ fst p
    mapM (\x -> putStrLn $ "\t" ++ show x) (snd p)
    return ()

printMenu :: IO ()
printMenu = do
    putStrLn ""
    putStrLn "*******************"
    putStrLn "* Commands        *"
    putStrLn "*******************"
    putStrLn "* 1 - Files       *"
    putStrLn "* 2 - Commodities *"
    putStrLn "* 3 - Query       *"
    putStrLn "* 4 - Exit        *"
    putStrLn "*******************"
    putStrLn "Enter choice:"

printHelper :: [(Int, String)] -> IO ()
printHelper [] = return ()
printHelper (x:xs) = do
    putStrLn $ show (fst x) ++ ") " ++ (snd x)
    printHelper xs

printFiles :: IO ()
printFiles = do
    files <- listDirectory dir
    putStrLn ""
    putStrLn "[Files]"
    printHelper $ zip [1..] $ map getDateFromFile files

printCommodities :: IO ()
printCommodities = do
    mapM putStrLn commodities
    return ()

printSearch :: IO ()
printSearch = do
    putStrLn ""
    putStrLn "Enter file to search: "
    file <- getLine
    putStrLn "Enter commodity: "
    commodity <- getLine
    if (elem commodity commodities) then searchCommodity file commodity else putStrLn "ERROR: Invalid commodity"
    return ()

searchCommodity :: String -> String -> IO ()
searchCommodity dt cm = do
    let filepath = (getFilePath . getFileFromDate) dt
    exists <- doesFileExist filepath
    if exists then searchCommodityInFile filepath cm 
    else putStr "File not found."
--    putStrLn $ "File exists: " ++ show exists
--    putStrLn $ "Loading " ++ filepath
--    putStrLn "Average price is..."
--    putStrLn "Lowest price is... in ..."
--    putStrLn "Highest price is... in ..."

searchCommodityInFile :: FilePath -> String -> IO ()
searchCommodityInFile f c = do
    withPdfFile f (searchAndDisplayResults c)

-- removes the filename prefix and extension to get the date
getDateFromFile :: String -> String
getDateFromFile s =
    let l = length s - 4
        p = length prefix
    in drop p $ take l s

-- prepends the filename prefix and appends the extension to the date 
getFileFromDate :: String -> String
getFileFromDate s = prefix ++ s ++ ".pdf"

-- prepends the directory where PDF files are stored, move this to StateMonad
getFilePath :: String -> String
getFilePath s = dir ++ "/" ++ s

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
            clearScreen
            printCommodities
            main
        "3" -> do
            printSearch
            main
        "4" -> do
            putStrLn "\nTHAT'S ALL FOLKS"
            return ()
        otherwise -> main
