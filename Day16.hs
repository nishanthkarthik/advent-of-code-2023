{-# LANGUAGE OverloadedStrings #-}

import qualified Data.Attoparsec.Text as At
import qualified Data.Attoparsec.Combinator as Atc
import qualified Data.Map.Lazy as M
import qualified Data.Set as S
import Data.Functor
import Control.Applicative
import Control.Monad
import Data.Bifunctor
import Data.Ix
import Control.Parallel.Strategies

import AocLib

type Idx = (Int, Int)
data Mirror = SplitV | SplitH | FSlash | BSlash | Empty deriving (Ord, Eq, Show)
type Mirrors = M.Map Idx Mirror

parser :: At.Parser (Mirrors,Idx)
parser = do
    let cell = At.choice [At.char '|' $> SplitV, At.char '-' $> SplitH,
                          At.char '\\' $> BSlash, At.char '/' $> FSlash,
                          At.char '.' $> Empty]
        row = zip [0..] <$> At.many1 cell
        rows = zip [0..] <$> At.sepBy1 row At.endOfLine
    rs <- rows
    let assocs = [((i, j), mirror) | (i, cells) <- rs, (j, mirror) <- cells, mirror /= Empty]
    return (M.fromList assocs, (length rs, length $ snd $ head rs))

-- Position with respect to the mirror -> Mirror -> direction vectors
passThrough :: Idx -> Mirror -> [Idx]
passThrough (i,j) m
    | m == BSlash = [(-j,-i)]
    | m == FSlash = [(j,i)]
    | m == SplitV && abs i == 1 = [(-i,j)]
    | m == SplitV && abs j == 1 = [(1,0),(-1,0)]
    | m == SplitH && abs j == 1 = [(i,-j)]
    | m == SplitH && abs i == 1 = [(0,-1),(0,1)]
    | otherwise = error "unreachable"

type RayHeads = S.Set (Idx,Idx) -- position, velocity
-- bounds -> mirrors -> cache -> working set -> all ray heads
step :: Idx -> Mirrors -> RayHeads -> RayHeads -> RayHeads
step bound m c ws
    | S.null ws = c
    | otherwise = step bound m (S.union ws c) (S.difference (S.difference newRays ws) c)
    where add (dx,dy) = bimap (dx +) (dy +)
          neg = bimap negate negate
          validate = filter (inRange ((0,0),bimap pred pred bound) . fst)
          pass (p,v) = let next = add p v in
                           if M.member next m
                                then map (stepRay (p,v)) $ passThrough (neg v) (m M.! next)
                                else [(next, v)]
          stepRay (p,v) delta = (add p v, delta)
          newRays = S.fromList . validate . concatMap pass . S.toList $ ws

showGrid :: Idx -> Mirrors -> RayHeads -> String
showGrid (ni,nj) m r = unlines [[paintIdx (i,j) | j <- [0..nj-1]] | i <- [0..ni-1]]
    where positions = S.map fst r
          paintIdx ix
              | M.member ix m = 'O'
              | S.member ix positions = '#'
              | otherwise = '.'

main :: IO ()
main = do
    (mirrors,bound) <- parseInput parser
    let solve1 = step bound mirrors S.empty
        printGrid = putStrLn . showGrid bound mirrors
        valid = inRange ((0,0),bound)
    let solve = S.size . S.filter valid . S.map fst . solve1 . S.singleton
    print $ solve ((0,-1),(0,1))

    let (li,lj) = bound
        vert = [[((i,-1),(0,1)), ((i,lj),(0,-1))] | i <- [0..li-1]]
        hori = [[((-1,j),(1,0)), ((li,j),(-1,0))] | j <- [0..lj-1]]
    print $ maximum $ parMap rpar solve $ concat $ vert ++ hori
