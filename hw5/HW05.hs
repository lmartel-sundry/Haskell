{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE OverloadedStrings, RecordWildCards #-}
module HW05 where

import Data.ByteString.Lazy (ByteString)
import Data.Map.Strict (Map)
import System.Environment (getArgs)

import qualified Data.ByteString.Lazy as BS
import qualified Data.Map.Strict as Map
import Data.Bits (xor)
import Data.List (sortBy)
import Control.Applicative

import Parser

-- Exercise 1 -----------------------------------------

getSecret :: FilePath -> FilePath -> IO ByteString
getSecret orig ciph = do
  origBytes <- BS.readFile orig
  ciphBytes <- BS.readFile ciph
  let xorBytes = BS.pack $ BS.zipWith xor origBytes ciphBytes
  return $ BS.filter (\b -> b /= 0) xorBytes

-- Exercise 2 -----------------------------------------

decryptWithKey :: ByteString -> FilePath -> IO ()
decryptWithKey key outfile = do
  encryptedBytes <- BS.readFile (outfile ++ ".enc")
  let copiesNeeded = ceiling $ (fromIntegral $ BS.length encryptedBytes) / (fromIntegral $ BS.length key)
  let longKey = BS.concat $ replicate copiesNeeded key
  let decryptedBytes = BS.pack $ BS.zipWith xor longKey encryptedBytes
  BS.writeFile outfile decryptedBytes

-- Exercise 3 -----------------------------------------

parseFile :: FromJSON a => FilePath -> IO (Maybe a)
parseFile infile = decode <$> BS.readFile infile

-- Exercise 4 -----------------------------------------

getBadTs :: FilePath -> FilePath -> IO (Maybe [Transaction])
getBadTs victimsPath transactionsPath = do
  victims <- parseFile victimsPath
  transactions <- parseFile transactionsPath
  return $ findBad victims transactions
    where findBad :: Maybe [String] -> Maybe [Transaction] -> Maybe [Transaction]
          findBad (Just vs) (Just ts) = Just $ filter (\t -> (tid t) `elem` vs) ts
          findBad _ _ = Nothing

-- Exercise 5 -----------------------------------------

getFlow :: [Transaction] -> Map String Integer
getFlow ts = Map.fromListWith (+) $ map (\t -> (to t, amount t)) ts

-- Exercise 6 -----------------------------------------

getCriminal :: Map String Integer -> String
getCriminal m = fst $ Map.foldrWithKey apprehend ("", -1) m
  where apprehend nextName nextAmt (curName, maxAmt)
          | nextAmt > maxAmt = (nextName, nextAmt)
          | otherwise        = (curName, maxAmt)

-- Exercise 7 -----------------------------------------

undoTs :: Map String Integer -> [TId] -> [Transaction]
undoTs totals tids = do
  let payers = sortDesc (<) totals
  let payees = sortDesc (>) totals
  let payments = zip payees payers
  if (length payments == 0)
    then []
    else do
        let newTs = map refund payments :: [Transaction]
        let newTotals = Map.filter (/= 0) . Map.mapWithKey (updateTotal newTs) $ totals
        newTs ++ (undoTs newTotals tids)
    where sortDesc :: (Integer -> Integer -> Bool) -> Map String Integer -> [(String, Integer)]
          sortDesc op m = sortBy desc . Map.toList $ Map.filter (op 0) m
          desc :: (a, Integer) -> (a, Integer) -> Ordering
          desc (_, l) (_, r)
            | (abs l) < (abs r) = GT
            | (abs l) > (abs r) = LT
            | otherwise         = EQ
          refund :: ((String, Integer), (String, Integer)) -> Transaction
          refund ((payee, surplus), (payer, deficit)) = Transaction payee payer (min surplus (- deficit)) ""
          updateTotal :: [Transaction] -> String -> Integer -> Integer
          updateTotal newTs name balance = balance + (paymentSum name to newTs) - (paymentSum name from newTs)
          paymentSum :: String -> (Transaction -> String) -> [Transaction] -> Integer
          paymentSum name direction ts = sum . map (\t -> amount t) . filter (\t -> (direction t) == name) $ ts



-- Exercise 8 -----------------------------------------

writeJSON :: ToJSON a => FilePath -> a -> IO ()
writeJSON outfile results = BS.writeFile outfile (encode results)

-- Exercise 9 -----------------------------------------

doEverything :: FilePath -> FilePath -> FilePath -> FilePath -> FilePath
             -> FilePath -> IO String
doEverything dog1 dog2 trans vict fids out = do
  key <- getSecret dog1 dog2
  decryptWithKey key vict
  mts <- getBadTs vict trans
  case mts of
    Nothing -> error "No Transactions"
    Just ts -> do
      mids <- parseFile fids
      case mids of
        Nothing  -> error "No ids"
        Just ids -> do
          let flow = getFlow ts
          writeJSON out (undoTs flow ids)
          return (getCriminal flow)

main :: IO ()
main = do
  args <- getArgs
  crim <-
    case args of
      dog1:dog2:trans:vict:ids:out:_ ->
          doEverything dog1 dog2 trans vict ids out
      _ -> doEverything "dog-original.jpg"
                        "dog.jpg"
                        "transactions.json"
                        "victims.json"
                        "new-ids.json"
                        "new-transactions.json"
  putStrLn crim
