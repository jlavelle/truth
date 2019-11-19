{-# LANGUAGE OverloadedStrings #-}

module Table where

import Data.Semigroup (stimesMonoid)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Lazy.Builder (Builder)
import qualified Data.Text.Lazy.Builder as B
import qualified Data.Text.Lazy.IO as LT
import qualified Data.Text.Lazy as LT

-- Simple ASCII tables

table :: [Text] -> [[Text]] -> LT.Text
table hs = LT.strip . B.toLazyText . table' hs

table' :: [Text] -> [[Text]] -> Builder
table' hs cs =
  let ws  = fmap T.length hs
      cws = fmap (zip ws) cs
  in header (zip ws hs) <> newline <> foldMap (\c -> row c <> newline) cws

header :: [(Int, Text)] -> Builder
header hs = hr hs <> newline <> row hs

row :: [(Int, Text)] -> Builder
row hs = bar <> foldMap (\(w, t) -> cell w t <> bar) hs <> newline <> hr hs

hr :: [(Int, a)] -> Builder
hr ws = corner <> foldMap go ws
  where
    go (n, _) = stimesMonoid (n + 2) (B.singleton '-') <> corner

cell :: Int -> Text -> Builder
cell w t =
  let pad = w - T.length t + 1
  in spaces 1 <> B.fromText t <> spaces pad

bar :: Builder
bar = B.singleton '|'

spaces :: Int -> Builder
spaces n = stimesMonoid n $ B.singleton ' '

corner :: Builder
corner = B.singleton '+'

newline :: Builder
newline = B.singleton '\n'

printBuilder :: Builder -> IO ()
printBuilder = LT.putStrLn . B.toLazyText
