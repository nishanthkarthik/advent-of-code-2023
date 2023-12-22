{-# LANGUAGE OverloadedStrings #-}

import qualified Data.Attoparsec.Text as At
import qualified Data.Attoparsec.Combinator as Atc
import qualified Data.Text as T
import qualified Data.Set as S
import Control.Applicative
import Control.Monad
import Data.Functor

import AocLib

type Idx = (Int, Int)
type Grid = S.Set Idx

parser :: At.Parser Grid
parser = do
    let cell = At.satisfy (At.inClass ".#")
        row = At.many1 cell
    rows <- At.sepBy1 row At.endOfLine
    let nr = length rows
        nc = length $ head rows
        assoc = zip (concat rows) [(i, j) | i <- [0..nr-1], j <- [0..nc-1]]
        galaxies = map snd $ filter ((== '#') . fst) assoc
    return (S.fromList galaxies)

emptyLines :: (Idx -> Int) -> Grid -> S.Set Int
emptyLines sel g = let ls = S.map sel g
                       emptyLs = S.difference (S.fromList [S.findMin ls..S.findMax ls]) ls
                   in emptyLs

expandUniv :: Int -> S.Set Int -> S.Set Int -> Idx -> Idx
expandUniv delta empR empC (i, j) = (i + rowsBefore, j + colsBefore)
    where rowsBefore = maybe 0 ((* delta) . succ . (`S.findIndex` empR)) (S.lookupLT i empR)
          colsBefore = maybe 0 ((* delta) . succ . (`S.findIndex` empC)) (S.lookupLT j empC)

sumShortestDist :: S.Set Idx -> Integer
sumShortestDist idxs
    | S.null idxs = 0
    | otherwise = sum dists + sumShortestDist next
    where (cur, next) =  S.deleteFindMin idxs
          dist (a, b) (c, d) = fromIntegral $ abs (a - c) + abs (b - d)
          dists = map (dist cur) (S.toList next)

main :: IO ()
main = do
    input <- parseInput parser
    let emptyRows = emptyLines fst input
        emptyCols = emptyLines snd input
    let univ1 = S.map (expandUniv (2 - 1) emptyRows emptyCols) input
        solve1 = sumShortestDist univ1
        univ2 = S.map (expandUniv (1000000 - 1) emptyRows emptyCols) input
        solve2 = sumShortestDist univ2
    print solve1
    print solve2
    return ()