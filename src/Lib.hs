{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances, OverloadedStrings #-}

module Lib
    ( someFunc
    ) where

import Data.Aeson
import Data.Aeson.Types
import qualified Data.HashMap.Strict as HM

someFunc :: IO ()
someFunc = putStrLn "someFunc"

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
