{-# LANGUAGE OverloadedStrings #-}

import AocLib

import qualified Data.Attoparsec.Text as At
import qualified Data.Attoparsec.Combinator as Atc
import qualified Data.Text as T
import qualified Data.Set as S
import Data.Functor
import Data.Bifunctor

type Dir = (Int, Int)
type Dist = Int
data Dig = Dig Dir Dist deriving (Eq, Show)
type Color = Int

parser :: At.Parser [(Dig, Color)]
parser = do
    let color = At.string "(#" *> At.hexadecimal <* At.char ')'
        dir = At.choice (map (\(c,d) -> At.char c $> d) [('R', (0,1)), ('L', (0,-1)), ('U', (-1,0)), ('D', (1,0))])
        line = (\d l c -> (Dig d l, c)) <$> (dir <* At.space) <*> (At.decimal <* At.space) <*> color
    At.sepBy1 line At.endOfLine

type Idx = (Int, Int)
type Ranges = [Idx]

digStep ::  Dig -> Idx -> Idx
digStep (Dig dir dist) ix = offset
    where scale = bimap (dist *) (dist *) dir
          offset = bimap (+ fst ix) (+ snd ix) scale

areaOfPolygon :: [Idx] -> Int
areaOfPolygon xs = abs . (`div` 2) . sum $ zipWith (\(x1,y1) (x2,y2) -> x1*y2 - x2*y1) xs $ tail xs

perimeter :: [Idx] -> Int
perimeter xs = sum nonCorners
    where nonCorners = zipWith (\(x1,y1) (x2,y2) -> abs (x1-x2) + abs (y1-y2)) xs $ tail xs

main :: IO ()
main = do
    input <- parseInput parser
    let steps = map fst input
        walk = scanl (flip digStep) (0,0)
    let occupiedCells w = areaOfPolygon w + perimeter w `div` 2 + 1 -- area = polyArea + bound / 2 + 1
    print $ occupiedCells $ walk steps

    let swapped = map (\(_, c) -> Dig (dirs !! mod c 16) (div c 16)) input
        dirs = [(0,1),(1,0),(0,-1),(-1,0)]
    print $ occupiedCells $ walk swapped
    return ()
