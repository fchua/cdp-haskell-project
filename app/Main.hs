module Main where

import Pdf.Core.File hiding (withPdfFile)
import Pdf.Document

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

main :: IO ()
main =
    withPdfFile "Price-Monitoring-June-10-2022.pdf" processMe
