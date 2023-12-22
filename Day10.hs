{-# LANGUAGE OverloadedStrings #-}

import qualified Data.Attoparsec.Text as At
import qualified Data.Attoparsec.Combinator as Atc
import qualified Data.Text as T
import Control.Applicative
import Control.Monad
import Data.Functor
import Data.Array
import Data.Maybe (isJust, fromMaybe, mapMaybe)
import Data.Ix
import Data.List (group)
import qualified Data.Set as S

import Debug.Trace

import AocLib

type GIx = (Int, Int)
type Grid = Array GIx Pipe

data Pipe = E | S | V | H | BL | BR | TL | TR deriving (Eq, Show, Enum)

charToPipe :: Char -> Pipe
charToPipe 'S' = S
charToPipe '-' = H
charToPipe '|' = V
charToPipe 'F' = TL
charToPipe '7' = TR
charToPipe 'L' = BL
charToPipe 'J' = BR
charToPipe _ = E

showCell :: Pipe -> Char
showCell E = '.'
showCell S = '*'
showCell V = '|'
showCell H = '-'
showCell BL = '└'
showCell BR = '┘'
showCell TL = '┌'
showCell TR = '┐'

showGrid :: Grid -> String
showGrid g = unlines [[showCell (g ! (j, i)) | i <- [ly..hy]] | j <- [lx..hx]]
    where ((lx, ly), (hx, hy)) = bounds g

validNeighbors :: Grid -> GIx -> Maybe [(GIx, [Pipe])]
validNeighbors grid ix@(i,j)
    | p == V = filterValid [up, down]
    | p == H = filterValid [left, right]
    | p == TL = filterValid [right, down]
    | p == TR = filterValid [left, down]
    | p == BL = filterValid [right, up]
    | p == BR = filterValid [left, up]
    | otherwise = Nothing
    where p = grid ! ix
          ofs di dj = (i + di, j + dj)
          left = (ofs 0 (-1), [H, TL, BL])
          right = (ofs 0 1, [H, TR, BR])
          up = (ofs (-1) 0, [TL, TR, V])
          down = (ofs 1 0, [BL, BR, V])
          validCell (ix, vals) = inRange (bounds grid) ix && grid ! ix `elem` vals
          filterValid as = if all validCell as then Just (filter validCell as) else Nothing

hasValidNeighbors :: Grid -> GIx -> Bool
hasValidNeighbors = (isJust .) . validNeighbors

parser :: At.Parser Grid
parser = do
    rows <- At.sepBy1 (At.many1 (At.notChar '\n')) At.endOfLine
    let cells = map (map charToPipe) rows
        nRows = length cells
        nCols = length $ head cells
    return $ listArray ((0, 0), (nRows - 1, nCols - 1)) (concat cells)

findStart :: Grid -> (Grid, GIx)
findStart grid = head $ do
    ix <- indices grid
    guard (grid ! ix == S)
    let neighbors = filter (`hasValidNeighbors` ix) $ map (\v -> grid // [(ix, v)]) [V .. TR]
    guard (not $ null neighbors)
    return (head neighbors, ix)

walkGrid :: Grid -> S.Set GIx -> S.Set GIx -> S.Set GIx
walkGrid grid curs visited = if S.null toVisit then S.union visited curs else walkGrid grid toVisit newSet
    where neighbors = S.fromList . map fst . concat $ mapMaybe (validNeighbors grid) (S.toList curs)
          newSet = S.union curs visited
          toVisit = S.difference neighbors newSet

enclosedCounts :: Grid -> Int
enclosedCounts grid = sum $ map (countEnclosedEmpty False) rows
    where b@((li,lj),(hi,hj)) = bounds grid
          rows = [[grid ! (i, j) | j <- [lj..hj]] | i <- [li..hi]]

countEnclosedEmpty :: Bool -> [Pipe] -> Int
countEnclosedEmpty _ [] = 0
countEnclosedEmpty False (V:xs) = countEnclosedEmpty True xs
countEnclosedEmpty True (V:xs) = countEnclosedEmpty False xs
countEnclosedEmpty True (E:xs) = 1 + countEnclosedEmpty True xs
countEnclosedEmpty st (_:xs) = countEnclosedEmpty st xs

vertWall :: Pipe -> Pipe
vertWall a
    {- NOTE Tricky bit
        We only count TL and TR because if a row has BL/BR and it also has potential cells
        which are enclosed by the polygon, then the same row also has to have TL and TR at
        some boundary. Counting only TL and TR lets us toggle the presence of a wall.
    -}
    | a `elem` [V, TL, TR] = V
    | otherwise = a

main :: IO ()
main = do
    input <- parseInput parser
    let printGrid = putStrLn . showGrid
    let (grid, st) = findStart input
    let walkPath = walkGrid grid (S.singleton st) S.empty
        walkedGrid = (E <$ grid) // zip (S.toList walkPath) (map (vertWall . (grid !)) $ S.toList walkPath)
    print (S.size walkPath `div` 2)
    print (enclosedCounts walkedGrid)