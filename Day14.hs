{-# LANGUAGE OverloadedStrings #-}

import qualified Data.Attoparsec.Text as At
import qualified Data.Attoparsec.Combinator as Atc
import qualified Data.Set as S
import Control.Monad
import Control.Applicative
import Data.Bifunctor
import Data.Ix
import Data.List
import Data.Maybe

import AocLib

type Idx = (Int, Int)

parser :: At.Parser (S.Set Idx, S.Set Idx, Idx) -- (fixed, rolling, upper bound)
parser = do
    let char = At.satisfy (At.inClass "O.#")
    rs <- At.sepBy1 (At.many1 char) At.endOfLine
    let m = length rs
        n = length $ head rs
        idxs = [(i, j) | i <- [0..m-1], j <- [0..n-1]]
        pairs = zip (concat rs) idxs
        pickIdxs c = S.fromList . map snd . filter ((== c) . fst)
    return (pickIdxs '#' pairs, pickIdxs 'O' pairs, (m, n))

roll :: (S.Set Idx -> (Idx, S.Set Idx)) -> (Idx -> Bool) -> (Idx -> Idx) -> S.Set Idx -> S.Set Idx -> S.Set Idx -> S.Set Idx
roll breakSet validIdx updateIdx fixed moved moving
    | S.null moving = moved
    | otherwise = roll breakSet validIdx updateIdx fixed (S.insert (last $ movements cur) moved) rest
    where (cur, rest) = breakSet moving
          notBlocked a = not (S.member a fixed) && not (S.member a moved) && not (S.member a rest)
          movements = takeWhile (\i -> validIdx i && notBlocked i) . iterate updateIdx

moment :: Idx -> S.Set Idx -> Int
moment (nr,_) = sum . map ((nr -) . fst) . S.toList

showGrid :: Idx -> S.Set Idx -> S.Set Idx -> String
showGrid bound fixed free = unlines [[showCell (i,j) | j <- [0..snd bound - 1]] | i <- [0..fst bound - 1]]
    where showCell ix
            | S.member ix fixed = '#'
            | S.member ix free = 'O'
            | otherwise = '.'

detectCycle :: (Eq a, Ord a) => [a] -> S.Set a -> [a]
detectCycle [] _ = []
detectCycle (x:xs) s
    | S.member x s = [x]
    | otherwise = x : detectCycle xs (S.insert x s)

main :: IO ()
main = do
    (fixed, free, upperBound) <- parseInput parser
    let validIdx = inRange ((0, 0), bimap pred pred upperBound)
        rollNorth = roll S.deleteFindMin validIdx (first pred) fixed S.empty
        rollSouth = roll S.deleteFindMax validIdx (first succ) fixed S.empty
        rollEast = roll S.deleteFindMax validIdx (second succ) fixed S.empty
        rollWest = roll S.deleteFindMin validIdx (second pred) fixed S.empty
        rollCycle = rollEast . rollSouth . rollWest . rollNorth
    let solve1 = rollNorth free
    print (moment upperBound solve1)
    let infiniteCycles = iterate rollCycle free
        n2 = 1000000000
        printGrid = putStrLn . showGrid upperBound fixed
        cyclePath = detectCycle infiniteCycles S.empty
        (fixedChunk, loopyChunk) = splitAt (fromJust $ elemIndex (last cyclePath) (init cyclePath)) (init cyclePath)
        selectedChunk = loopyChunk !! ((n2 - length fixedChunk) `mod` length loopyChunk)
    print (moment upperBound selectedChunk)
