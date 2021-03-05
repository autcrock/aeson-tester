{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

module Lib
    ( aesonTest,
      parseReferers,
      inputData,
      doIt,
      Referer
    ) where

import Data.Aeson
import Data.Aeson.Types
import Data.Either.Unwrap (isLeft, fromLeft, fromRight)
import qualified Data.HashMap.Strict as HM
import qualified Data.ByteString.Lazy.Char8 as DBSLC8

aesonTest :: IO ()
aesonTest = putStrLn inputData

inputData :: String
inputData =
   "{\
    \\"website1.com\":\n\
    \  {\"/page1\": 3,\n\
    \   \"/page2\": 4}\n\
    \,\"website2.com\":\n\
    \  {\"/page\": 10}\n\
    \}"

data Referer = Referer {
  domain       :: String,
  pathAccesses :: [(String, Int)] }
  deriving (Show)

parseReferers :: Value -> Parser [Referer]
parseReferers p =
  -- Convert each “accesses” object to a list of pairs, and create a Referrer.
  map (\(domain, accesses) -> Referer domain (HM.toList accesses)) .
  -- Turn the HashMap into a list of (domain, accesses) pairs.
  -- Each “accesses” object looks like {"/page1": 3, ...}.
  HM.toList <$>
  -- Parse our data into a HashMap String (HashMap String Int).
  parseJSON p


-- readMapFromString :: String -> Map
-- readMapFromString candidateMap =
--     let
--         eitherMap = eitherDecode (DBSLC8.pack candidateMap) :: (Either String Map)
--     in
--         if isLeft eitherMap 
--             then 
--                 Map { MapDefinitions.map = [] }
--             else
--                 fromRight eitherMap

doIt =
  let
    value = eitherDecode $ DBSLC8.pack inputData :: Either String Value
    parsed = 
      if isLeft value then error $ fromLeft value
      else parseReferers $ fromRight value
  in
    parsed
