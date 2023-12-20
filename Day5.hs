{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

import qualified Data.Attoparsec.Text as At
import qualified Data.Attoparsec.Combinator as Atc
import qualified Data.Text as T
import qualified Data.IntSet as IS
import Control.Applicative
import Control.Monad
import Data.Functor
import Data.Bifunctor (bimap)
import Data.Maybe (isJust, fromJust, catMaybes)
import Data.Ix (inRange)
import Data.List.Split (chunksOf)
import Data.List (sort)

import AocLib

type Seeds = [Int]

data Mapping = Mapping { dStart :: Int, dEnd :: Int, sStart :: Int, sEnd :: Int } deriving Show

mappingFn :: [Mapping] -> Int -> Int
mappingFn ms key = if null candidates then key else offset $ head candidates
    where keyInRange (Mapping ds de ss se) = ss <= key && key < se
          candidates = filter keyInRange ms
          offset (Mapping ds de ss se) = key - ss + ds

parser :: At.Parser (Seeds, [[Mapping]])
parser = do
    let parseMapLine = do
            ds <- At.decimal <* At.space
            ss <- At.decimal <* At.space
            l <- pred <$> At.decimal
            return (Mapping ds (ds + l) ss (ss + l))
        identifier = At.many1 (At.letter <|> At.char '-')
        parseMap = do
            identifier <* At.string " map:" <* At.endOfLine
            At.many1 (parseMapLine <* At.endOfLine)
    seeds <- At.string "seeds: " *> At.sepBy1 At.decimal At.space <* At.many' At.endOfLine
    mappings <- At.sepBy1 parseMap At.endOfLine
    return (seeds, mappings)

type Range = (Int, Int)

splitRange :: Range -> Range -> (Maybe Range, [Range])
splitRange (a, m) (b, n) -- a is reference to split
    | inRange (a, m) b || inRange (b, n) a = (Just (max a b, min m n), restOfChunks)
    | otherwise = (Nothing, [(a, m)])
    where validRange (l, r) = l <= r && l >= a && r <= m
          restOfChunks = filter validRange [(min a b, max a b - 1), (min m n + 1, max m n)]


breakMappings :: [Mapping] -> [Range] -> [Range]
breakMappings ms as = let (ov, no) = foldr breakMapping ([], as) ms
                      in ov ++ no

-- (overlapping range, non-overlapping range)
breakMapping :: Mapping -> ([Range], [Range]) -> ([Range], [Range])
breakMapping (Mapping dS _ sS sE) (ov, no) = let match = map (`splitRange` (sS, sE)) no
                                                 (ov1, no1) = bimap catMaybes concat $ unzip match
                                                 mapFn a = a - sS + dS
                                                 mapRange = bimap mapFn mapFn
                                                 mapOv = map mapRange ov1
                                             in (ov ++ mapOv, no1)

main :: IO ()
main = do
    (seeds, mappings) <- parseInput parser
    let lookupFn = foldl1 (flip (.)) $ map mappingFn mappings
        solve1 = minimum $ map lookupFn seeds
    print solve1

    let seedranges = map (\[a, l] -> (a, a + l - 1)) $ chunksOf 2 seeds
        ranges = foldl (flip breakMappings) seedranges mappings
        solve2 = fst $ minimum ranges

    print solve2
