{-# LANGUAGE DeriveGeneric #-}
{-#LANGUAGE OverloadedStrings #-}
--Magic scripts--
---------------------------------------
module Main where
import Text.HTML.TagSoup
import System.Directory
import Control.Monad
import System.IO
import Data.List
import Data.Maybe
import qualified Data.ByteString.Lazy.Char8
import qualified Data.ByteString.Lazy as B
import GHC.Generics
import Data.Aeson
--Data "classes" which everyone uses--
---------------------------------------
data WebPage = WebPage {url :: String, html_content :: String} deriving (Show, Generic)

data WebPageRank = WebPageRank {urlP :: String, pagerank :: Double} deriving (Show, Generic)

getPageRank :: WebPageRank -> Double
getPageRank (WebPageRank _ pagerank) = pagerank

getUrlPageRank :: WebPageRank -> String
getUrlPageRank (WebPageRank urlP _) = urlP

getHmtlContent :: WebPage -> String
getHmtlContent (WebPage _ html_content) = html_content

getUrl :: WebPage -> String
getUrl (WebPage url _) = url

instance FromJSON WebPageRank where
    parseJSON (Object v) = WebPageRank <$> v .: "url" <*> v .: "pagerank"
    parseJSON _ = mempty

instance ToJSON WebPageRank where
    toJSON (WebPageRank urlP pagerank) = object ["url" .= urlP, "pagerank" .= pagerank]

instance FromJSON WebPage where
    parseJSON (Object v) = WebPage <$> v .: "url" <*> v .: "html_content"
    parseJSON _ = mempty

instance ToJSON WebPage where
    toJSON (WebPage url html_content) = object ["url" .= url, "html_content" .= html_content]
---------------------------------------

--Sabina Daniela Pekareková--
---------------------------------------
findString :: (Eq a) => [a] -> [a] -> Maybe Int
findString search str = findIndex (isPrefixOf search) (tails str)

getTag :: String -> String -> String -> Maybe String
getTag handle tag1 tag2 = case findString tag1 handle of
    (Just val1) -> case getCloseTag (snd (splitAt val1 handle)) tag2 of
      (Just a) -> Just a
      Nothing -> Nothing
    Nothing -> Nothing

getCloseTag :: String -> String -> Maybe String
getCloseTag handle tag2 = case findString tag2 handle of
   (Just a) -> Just $ fst $ splitAt a handle
   _ -> Nothing

delTag :: String -> String -> String -> String
delTag handle tag1 tag2 = case (findString tag1 handle, findString tag2 handle) of
    ((Just val1), (Just val2)) -> (fst (splitAt val1 handle)) ++ (delTag (snd (splitAt (val2 + (length tag2)) handle)) tag1 tag2)
    _ -> handle

getJSON :: IO B.ByteString
getJSON = B.readFile "output.jl"

lazyByteString :: String -> B.ByteString
lazyByteString str = Data.ByteString.Lazy.Char8.pack str

getParsedFile :: Maybe WebPage -> (String, String)
getParsedFile page = case page of
  Just wp -> case (getTag (getHmtlContent wp) "<body" "</body>") of
    Just val -> (getUrl wp, innerText $ parseTags $ delTag (delTag val "<script" "</script>") "<style" "</style>")
    Nothing -> (" ", " ")
  Nothing -> (" ", " ")
---------------------------------------

--Filip Michal Gajdoš--
---------------------------------------

{-
findString :: (Eq a) => [a] -> [a] -> Maybe Int
findString search str = findIndex (isPrefixOf search) (tails str)
-}

findWord :: [Char] -> [Char] -> Bool
findWord arg text = case findString (" " ++ arg ++ " ") text of
  Just value -> True
  Nothing -> False

addNumbers :: [(String, String)] -> [(Int, (String, String))]
addNumbers list = zip [1..] list

writeFnum :: (Show a) => a -> IO ()
writeFnum num = appendFile "inverted_index.txt" $ show num

writeF :: String -> IO ()
writeF text = appendFile "inverted_index.txt" $ text

prepare_inv_index :: String -> IO ()
prepare_inv_index arg = do
    writeF (arg ++ " [")
    fl <- readFile $ "output.jl"
    flp <- readFile $ "pagerank.jsonl"
    forM_ (addNumbers (map getParsedFile (map decode (map lazyByteString (lines fl)) :: [Maybe WebPage]))) $ \parsed_tuple -> case findWord arg (snd (snd parsed_tuple)) of
      True -> do
        writeF ("(") >> writeFnum (fst parsed_tuple) >> writeF (",")
        case ((map decode (map lazyByteString (lines flp)) :: [Maybe WebPageRank]) !! ((fst parsed_tuple)-1)) of
          Just val -> writeFnum (getPageRank val)
          Nothing -> writeFnum 0
        writeF ("),")
      False -> return ()
    writeF ("(-1,-1)]\n")

rList :: String -> [(Int, Double)]
rList = read

inverted_index :: IO ()
inverted_index = do
  content <- readFile "./inverted_index.txt"
  fl <- readFile $ "output.jl"
  let wPages = map decode (map lazyByteString (lines fl)) :: [Maybe WebPage]
  let numbered_parsed_pages = addNumbers $ map getParsedFile wPages
  writeFile "test.txt" ""
  forM_ (lines content) $ \line -> do
    putStr (((words line) !! 0) ++ " ") >> appendFile "test.txt" (show (init (reverse (sortOn snd (rList ((words line) !! 1)))))) >> appendFile "test.txt" " "
  putStrLn ""
  final_file <- readFile "./test.txt"
  forM_ (lines final_file) $ \line -> do
    let intersection = foldl1 intersect $ map rList (words line)
    if intersection == [] then putStrLn "slovo/a sa nenachadzaju na ziadnej stranke"
    else do
      putStrLn "slovo/a sa nachadzaju na strankach"
      forM_ intersection $ \tuple -> do
        putStr ("(" ++ (fst (snd (numbered_parsed_pages !! ((fst tuple)-1)))) ++ ", " ++ (show (snd tuple)) ++ "), ")
      putStrLn ""
  removeFile "inverted_index.txt" >> removeFile "test.txt"
---------------------------------------

--Main?--
---------------------------------------
main :: IO ()
main = do
  putStrLn "Zadaj hladane slova" >> writeFile "./inverted_index.txt" ""
  input <- getLine
  putStrLn "---------"
  forM_ (words input) $ \arg -> do
    prepare_inv_index arg
  inverted_index
