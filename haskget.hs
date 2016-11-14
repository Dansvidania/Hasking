module HaskGet(curl) where 

import System.Process
import Data.Char

curl :: String -> IO String
curl url = readProcess "curl" [url] []

version = 1.1

data OrderType = Ask | Bid deriving (Read)
instance Show OrderType where
        show Ask = "Ask"
        show Bid = "Bid"

data Trade = Trade { pair :: String
           , orderType :: OrderType
           , price :: Double
           , amount :: Double
           , id :: Int
           , timeStamp :: Int } deriving (Show, Read)

buildTrade :: String -> [String] -> Trade
buildTrade pair (orderTypeStr:priceStr:amountStr:idStr:timeStampStr:[]) =
        Trade pair orderType price amount id timeStamp
        where orderType = read $ capitalize orderTypeStr :: OrderType
              price = read priceStr :: Double
              amount = read amountStr :: Double
              id = read idStr :: Int
              timeStamp = read timeStampStr :: Int

capitalize :: String -> String
capitalize (x:xs) = toUpper x : xs

groupsOf :: Int -> [a] -> [[a]]
groupsOf 0 _ = undefined
groupsOf _ [] = []
groupsOf n xs = take n xs : groupsOf n (drop n xs)

changeChar :: String -> Char -> Char -> String
changeChar [] _ _ = []
changeChar (x:ss) from to = 
        if (x /= from)
            then x : changeChar ss from to
            else to : changeChar ss from to

parseTrades :: String -> [[String]] -> [Trade]
parseTrades _ [] = []
parseTrades "" _ = undefined
parseTrades pair (t:ts) = buildTrade pair t : parseTrades pair ts       

parseResponse :: String -> [Trade]
parseResponse response = 
        parseTrades pair $ trades
        where pair = "btc_usd" --take 1 cleanResponseWords
              trades = groupsOf 5 $ dropOddIndexs $ drop 1 $ words $ response

dropOddIndexs :: [a] -> [a]
dropOddIndexs (x:y:as) = y:dropOddIndexs as
dropOddIndexs any = any

cleanJsonResponse :: String -> String
cleanJsonResponse [] = []
cleanJsonResponse (x:xs) =
        if ( x `elem` savedChars)
            then x : cleanJsonResponse xs
            else space : cleanJsonResponse xs
        where space = ' '
              savedChars = ['.', '_'] ++ ['0'..'9'] ++ ['a'..'z']

main = do
        response <- curl "https://btc-e.com/api/3/trades/btc_usd"
        let cleanResp = cleanJsonResponse response
            trades = parseResponse cleanResp
            trade = trades !! 1
            readTrade = read $ show trade :: Trade
        return trades
