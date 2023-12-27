{-# LANGUAGE OverloadedStrings #-}

import AocLib

import qualified Data.Attoparsec.Text as At
import qualified Data.Attoparsec.Combinator as Atc
import qualified Data.Text as T
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.Sequence as Seq
import qualified Data.Array as A
import Data.List
import Data.Maybe
import Data.Function (on)
import Data.Functor
import Data.Bifunctor
import Control.Applicative
import Control.Parallel.Strategies
import Data.Containers.ListUtils (nubOrd)

data Cell = Open | U | D | L | R deriving (Eq, Ord, Show)

type Idx = (Int,Int)
type Grid = M.Map Idx Cell

parser :: At.Parser (Grid,Idx)
parser = do
    let cell = At.satisfy $ At.inClass "#.^<>v"
        row = zip [0..] <$> At.many1 cell
        charToCell '^' = U
        charToCell 'v' = D
        charToCell '<' = L
        charToCell '>' = R
        charToCell _ = Open
    rs <- zip [0..] <$> At.sepBy1 row At.endOfLine
    let ascs = [((i,j),charToCell c) | (i,r) <- rs, (j,c) <- r, c /= '#']
        nr = length rs
        nc = length $ snd $ head rs
    return (M.fromList ascs, (nr,nc))

dirDeltas :: Cell -> [Idx]
dirDeltas Open = [(0,1),(1,0),(0,-1),(-1,0)]
dirDeltas U = [(-1,0)]
dirDeltas D = [(1,0)]
dirDeltas L = [(0,-1)]
dirDeltas R = [(0,1)]

walk1 :: Grid -> Idx -> Idx -> Int
walk1 grid start end = run S.empty start
    where run :: S.Set Idx -> Idx -> Int
          run visited cur@(i,j)
              | cur == end = S.size visited
              | otherwise = let deltas = map (bimap (i +) (j +)) $ dirDeltas (grid M.! cur)
                                valid = filter (liftA2 (&&) (`M.member` grid) (not . (`S.member` visited))) deltas
                                visited' = S.insert cur visited
                            in if null valid then -1 else maximum $ map (run visited') valid

walk2 :: Grid -> Idx -> S.Set Idx -> [(Int,Idx)]
walk2 grid start ends = run S.empty start
    where run :: S.Set Idx -> Idx -> [(Int,Idx)]
          run visited cur@(i,j)
              | S.member cur ends = [(S.size visited, cur)]
              | otherwise = let deltas = map (bimap (i +) (j +)) $ dirDeltas (grid M.! cur)
                                valid = filter (liftA2 (&&) (`M.member` grid) (not . (`S.member` visited))) deltas
                                visited' = S.insert cur visited
                            in if null valid then [] else concatMap (run visited') valid

type Graph = M.Map Int [(Int,Int)]
dfs2 :: Graph -> Int -> Int -> Int
dfs2 graph start end = run S.empty start
    where run :: S.Set Int -> Int -> Int
          run visited cur
              | cur == end = 0
              | otherwise = let nexts = filter (not . (`S.member` visited) . fst) $ graph M.! cur
                                rec next@(ix,dist) = dist + run (S.insert cur visited) ix
                            in if null nexts then minBound else maximum $ parMap rpar rec nexts

main :: IO ()
main = do
    (grid,n@(nr,nc)) <- parseInput parser
    let start = (0,1) :: Idx
        end = (nr-1,nc-2) :: Idx
        solve1 = walk1 grid start end
    print solve1
    let grid2 = M.map (const Open) grid
        isFork ix@(i,j) = let neighbors = map (bimap (i+) (j+)) $ dirDeltas Open
                              valid = filter (`M.member` grid2) neighbors
                          in M.member ix grid2 && (length valid > 2)
        forks = filter isFork $ M.keys grid2
        starts = forks ++ [start, end]
        forkDistances = concat [[(a,[(b,d)]),(b,[(a,d)])] | a <- starts, (d,b) <- walk2 grid2 a (S.delete a (S.fromList starts)) `using` rpar]
        graph = nubOrd <$> M.fromListWith (++) forkDistances
        indexedKeys = zip (M.keys graph) [0..]
        keyToIdx = fromJust . (`lookup` indexedKeys)
        indexedGraph = map (first keyToIdx) <$> M.mapKeys keyToIdx graph
        solve2 = dfs2 indexedGraph (keyToIdx start) (keyToIdx end)
    print solve2
