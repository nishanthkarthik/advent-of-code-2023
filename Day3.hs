{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

import qualified Data.Attoparsec.Text as At
import qualified Data.Attoparsec.Combinator as Atc
import qualified Data.Text as T
import Control.Applicative
import Control.Monad
import Data.Bifunctor (first, second, bimap)
import Data.Functor
import Data.Array
import Data.Char (digitToInt)
import qualified Data.Set as S

import AocLib

data Cell = Empty | Symbol | Gear | Value {value :: Int} deriving (Show, Eq)
type GIx = (Int, Int)
type Grid = Array GIx Cell

isValueCell :: Cell -> Bool
isValueCell (Value _) = True
isValueCell _ = False

parser :: At.Parser Grid
parser = do
    let parseCell = At.choice [Value . digitToInt <$> At.digit,
                               At.char '.' $> Empty,
                               At.char '*' $> Gear,
                               At.notChar '\n' $> Symbol]
    cells <- At.sepBy1 (At.many1 parseCell) At.endOfLine
    let rows = length cells
        cols = length $ head cells
    return $ listArray ((0, 0), (rows - 1, cols - 1)) (concat cells)

type NumPosition = (Int, S.Set GIx)

extractNums :: Grid -> [NumPosition]
extractNums grid = do
    (ir, ic) <- range $ bounds grid
    let ix = (ir, ic)
    guard (isValueCell $ grid ! ix)
    let leftix = (ir, ic - 1)
        validIx = inRange $ bounds grid
    guard (not (validIx leftix) || not (isValueCell (grid ! leftix)))
    let rightixs = takeWhile (\it -> validIx it && isValueCell (grid ! it)) $ map (ir, ) [ic+1..]
        digits = map (value . (grid !)) (ix : rightixs)
        asNum = foldl1 ((+) . (* 10)) digits
    return (asNum, S.fromList (ix : rightixs))

solve1 :: [NumPosition] -> Grid -> Int
solve1 nums grid = sum $ do
    (n, ixs) <- nums
    let isValidIx = inRange (bounds grid)
        left = second pred $ S.findMin ixs
        right = second succ $ S.findMax ixs
        up = map (first pred) (left : right : S.toList ixs)
        down = map (first succ) (left : right : S.toList ixs)
    guard (any ((== Symbol) . (grid !)) (filter isValidIx ([left, right] ++ up ++ down)))
    return n

solve2 :: [NumPosition] -> Grid -> Int
solve2 nums grid = sum $ do
    let bs = bounds grid
    ix@(ir, ic) <- range bs
    guard (grid ! ix == Gear)
    let dixs = S.fromList $ filter (inRange bs) [bimap (+ di) (+ dj) ix | di <- [-1..1], dj <- [-1..1], (di, dj) /= (0, 0)]
        matchingNums = filter (not . S.null . S.intersection dixs . snd) nums
    guard (length matchingNums == 2)
    return $ product (map fst matchingNums)

main :: IO ()
main = do
    input <- parseInput parser
    let numbers = extractNums input
    print (solve1 numbers input)
    print (solve2 numbers input)
