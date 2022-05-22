{-# LANGUAGE DeriveGeneric #-}
{-#LANGUAGE OverloadedStrings #-}
--Magic scripts--
---------------------------------------
module Main where
import Text.HTML.TagSoup
import System.Directory
import Control.Monad
import System.IO
import Data.List.Split
import Data.List
import Data.Maybe
import qualified Data.ByteString.Lazy.Char8
import qualified Data.ByteString.Lazy as B
import GHC.Generics
import Data.Aeson
import Data.Char
import qualified Data.Text.Encoding as E

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
------------------------------------------------
-- Snaha o odstranenie unicode charov z textu --
{-
rInt :: String -> Int
rInt = read

replaceAll :: String -> String
replaceAll str = concat $ map (\s -> replaceSpecial Nothing s) (splitOn "\\" str)

replaceSpecial :: Maybe Int -> String -> String
replaceSpecial Nothing []  = []
replaceSpecial (Just a) []  = (chr a):[]
replaceSpecial (Just a) (x:xs)
  | isDigit x = replaceSpecial (Just (a * 10 + (digitToInt x))) xs
  | otherwise = (chr a):x:xs
replaceSpecial Nothing (x:xs)
  | isDigit x = replaceSpecial (Just (digitToInt x)) xs
  | otherwise = x:xs
-}
------------------------------------------------
---------------------------------------
--Filip Michal Gajdoš--
---------------------------------------
findWordNew :: String -> ((Int, Double), (String, String)) -> (Double, String)
findWordNew arg page = case findString (" " ++ arg ++ " ") (snd ( snd page)) of
  Just value -> (snd (fst page), fst (snd page))
  Nothing -> (0.0, "None")

getWebPages :: String -> [(String, String)]
getWebPages file = map getParsedFile (map decode (map lazyByteString (lines file)) :: [Maybe WebPage])

getWebPageRanks :: String -> [Maybe WebPageRank]
getWebPageRanks file = map decode (map lazyByteString (lines file)) :: [Maybe WebPageRank]

prepare_inv_index :: String -> [(Int, Double)] -> String -> [(Double, String)]
prepare_inv_index arg  idxAndPageRank file = filter (/=(0.0,"None")) $ zipWith findWordNew (take (length (lines file)) (iterate (++"") arg)) (zip idxAndPageRank (getWebPages file))

---------------------------------------
--Michaela Polakova--
---------------------------------------
appFile :: String -> IO()
appFile url = case findString "https:" url of
    Just val -> appendFile "parserOutputPages.txt" (", " ++ (snd (splitAt (val) url)))
    Nothing -> appendFile "parserOutputPages.txt" ""

mapPage :: Maybe WebPage -> IO ()
mapPage mpage = case mpage of
  (Just val) -> do
     appendFile "parserOutputPages.txt" $ getUrl val
     mapM appFile $ map(fromAttrib "href") $ filter (~== TagOpen ("a"::String) []) $ parseTags $ getHmtlContent val
     appendFile "parserOutputPages.txt" " \n"
  Nothing -> appendFile "parserOutputPages.txt" ""

parsePages :: String -> IO [()]
parsePages fl = writeFile "parserOutputPages.txt" "" >> mapM mapPage (map decode (map lazyByteString (lines fl)) :: [Maybe WebPage])

---------------------------------------
--Main--
---------------------------------------
main :: IO ()
main = do
  print "Vytvaranie URL ref..."
  mainFile <- readFile "output.jl"
  parsePages mainFile
  print "Hotovo"
  putStrLn "----------------"
  putStrLn "Zadaj hladane slova" >> writeFile "./inverted_index.txt" ""
  input <- getLine
  pageranks <- readFile "pagerank.jsonl"
  let indxAndPageRank = (take (length (lines mainFile)) (zip [1..] (map getPageRank (map fromJust (getWebPageRanks pageranks)))))
  putStrLn "----------------"
  let intersection = foldl1 intersect (zipWith3 prepare_inv_index (words input) (replicate (length (words input)) indxAndPageRank) (replicate (length (words input)) mainFile))
  if intersection == [] then print "Vsetky slovo/a sa nenachadzaju v ziadnej stranke"
  else do
    putStrLn "slovo/a sa nachadzaju v strankach:"
    print $ reverse $ sortOn fst $ take 10 intersection