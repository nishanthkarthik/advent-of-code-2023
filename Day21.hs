{-# LANGUAGE OverloadedStrings #-}

import AocLib

import qualified Data.Attoparsec.Text as At
import qualified Data.Attoparsec.Combinator as Atc
import qualified Data.Text as T
import qualified Data.Map as M
import qualified Data.Set as S
import Data.Bifunctor
import Data.Ix

type Idx = (Int,Int)
type Grid = S.Set Idx

parser :: At.Parser (Grid, Idx, Idx)
parser = do
    let cell = At.choice $ map At.char ".#S"
        row = zip [0..] <$> At.many1 cell
    rs <- zip [0..] <$> At.sepBy1 row At.endOfLine
    let start = fst $ head $ filter ((== 'S') . snd) assocs
        assocs = [((i,j),v) | (i,r) <- rs, (j,v) <- r, v /= '.']
        nr = length rs
        nc = length $ snd $ head rs
    return (S.delete start (S.fromList $ map fst assocs), start, (nr,nc))

data Dist = Dist Int | Inf deriving (Eq, Show)

instance Ord Dist where
    compare (Dist a) (Dist b) = compare a b
    compare (Dist a) Inf = LT
    compare Inf (Dist a) = GT
    compare Inf Inf = EQ

type Vertex = Idx
type GetNeighbors = Vertex -> [(Vertex, Dist)]
type DistMap = M.Map Vertex Dist
type PQueue = S.Set (Dist, Vertex)

shortestPath :: GetNeighbors -> Vertex -> DistMap
shortestPath getNeighbors start = fn initialQueue initialDistMap initialVisited
    where initialDistMap = M.singleton start (Dist 0)
          initialQueue = S.singleton (Dist 0, start)
          initialVisited = S.empty
          fn :: PQueue -> DistMap -> S.Set Vertex -> DistMap
          fn qu dm vis
              | S.null qu = dm
              | otherwise = let ((Dist distU, vertU), restQu) = S.deleteFindMin qu
                                edges = filter (not . (`S.member` vis) . fst) $ getNeighbors vertU
                                safeLookupDist vert = if M.member vert dm then dm M.! vert else Inf
                                edgeDists = map (\(v, Dist dv) -> min (safeLookupDist v) (Dist (distU + dv))) edges
                                assocs = zip edgeDists $ map fst edges
                                newdm = foldr (\(d, v) ac -> M.insert v d ac) dm assocs
                                newqu = foldr S.insert restQu assocs
                                newvis = S.insert vertU vis
                            in fn newqu newdm newvis

getNeighbors1 :: Grid -> Idx -> Vertex -> [(Vertex, Dist)]
getNeighbors1 grid bound (i,j) = flip zip (repeat $ Dist 1) $ filter free $ filter valid deltas
    where valid (di,dj) = inRange ((0,0), bimap pred pred bound) (di,dj)
          free = not . (`S.member` grid)
          deltas = [(i+di,j+dj) | (di,dj) <- [(1,0),(-1,0),(0,1),(0,-1)]]

showGrid :: Grid -> Idx -> S.Set Vertex -> String
showGrid grid (nr,nc) coords = unlines [[paint (i,j) | j <- [0..nc-1]] | i <- [0..nr-1]]
    where paint ix
            | S.member ix grid = '#'
            | S.member ix coords = 'O'
            | otherwise = '.'

main :: IO ()
main = do
    (grid,start,bound) <- parseInput parser
    let distMap = shortestPath (getNeighbors1 grid bound)
        printGrid = putStrLn . showGrid grid bound
        path n st = S.fromList $ map fst $ filter (\(k,Dist v) -> v <= n && mod v 2 == mod n 2) $ M.toList $ distMap st
        path0 n = S.fromList $ map fst $ filter (\(k,Dist v) -> v <= n && mod v 2 == mod n 2) $ M.toList $ distMap start
        nSteps n = S.size $ path0 n
    print $ nSteps 64

    let oddgrid = nSteps 201
        evengrid = nSteps 200
        n0 = 26501365
        n = (n0 - fst start) `div` fst bound
        innerOdd = (n + 1) + n * (n + 1)
        innerEven = n + n * (n - 1)
        oddOuterDiamond = nSteps 251 - nSteps 65
        -- WRONG! there's a straggler 'O' in the even inner diamond that this does not track
        -- evenOuterDiamond = nSteps 250 - nSteps 64 - 1
        -- calculate the four corners explicitly
        evenCorners = sum $ map (S.size . path 64) [(0,0),(130,0),(0,130),(130,130)]
        total = innerOdd * oddgrid
                + innerEven * evengrid
                - (n + 1) * oddOuterDiamond
                + n * evenCorners
    print total
