{-# LANGUAGE OverloadedStrings #-}

import qualified Data.Attoparsec.Text as At
import qualified Data.Attoparsec.Combinator as Atc
import qualified Data.Text as T
import qualified Data.IntSet as IS
import Control.Applicative
import Control.Monad
import Data.Functor
import Data.Bits
import Data.List (intersect)

import Debug.Trace

import AocLib

data Card = Card{ cardNum :: Int, matches :: Int } deriving (Show)

parser :: At.Parser [Card]
parser = flip At.sepBy1 At.endOfLine $ do
    n <- At.string "Card" *> At.skipSpace *> At.decimal <* At.string ":"
    let manyNums = At.skipSpace *> At.sepBy1 At.decimal At.skipSpace
    wins <- manyNums
    At.many' (At.skipSpace *> At.char '|')
    cur <- manyNums
    return (Card n (length (cur `intersect` wins)))

main :: IO ()
main = do
    cards <- map matches <$> parseInput parser
    let solve1 = sum $ map (pow2 . pred) $ filter (> 0) cards
        pow2 = shiftL (1 :: Int)
    print solve1
    let n = length cards
        as = reverse cards
        solve2 = [1 + (sum . map (solve2 !!) . filter (>= 0) $ map (i -) [1..as !! i]) | i <- [0..n-1]]
    print (sum solve2)