{-# LANGUAGE OverloadedStrings #-}

import AocLib

import qualified Data.Attoparsec.Text as At
import qualified Data.Attoparsec.Combinator as Atc
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import Data.Char (digitToInt)
import Data.Bifunctor

import Debug.Trace

parser :: At.Parser GridWeights
parser = do
    let row = zip [0..] <$> At.many1 (digitToInt <$> At.digit)
        rows = zip [0..] <$> At.sepBy1 row At.endOfLine
    rs <- rows
    let assocs = [((i,j),v) | (i,r) <- rs, (j,v) <- r]
    return $ M.fromList assocs

type Idx = (Int, Int)
type Dir = (Int, Int)
type Vertex = (Idx, Dir)

data Dist = Dist Int | Inf deriving (Eq, Show)

instance Ord Dist where
    compare (Dist a) (Dist b) = compare a b
    compare (Dist a) Inf = LT
    compare Inf (Dist a) = GT
    compare Inf Inf = EQ

type DistMap = M.Map Vertex Dist
type PQueue = S.Set (Dist, Vertex)

type GridWeights = M.Map Idx Int

nearEdges :: GridWeights -> (Int, Int) -> Vertex -> [(Vertex, Dist)]
nearEdges grid (takeN, dropN) (ix, dir) = nexts
    where valid = flip M.member grid
          dirs (i,j) = [(j, i), (negate j, negate i)]
          step = bimap (+ fst dir) (+ snd dir)
          steps = takeWhile valid $ tail $ iterate step ix
          dists = scanl1 (+) $ map (grid M.!) steps
          candidates = take takeN $ drop dropN $ zip steps dists
          nexts = [((sidx, d), Dist dist) | (sidx, dist) <- candidates, d <- dirs dir]

type GetNeighbors = Vertex -> [(Vertex, Dist)]

shortestPath :: GetNeighbors -> Vertex -> Vertex -> Dist
shortestPath getNeighbors start end = fn initialQueue initialDistMap initialVisited M.! end
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

main :: IO ()
main = do
    weights <- parseInput parser
    let start = fst $ M.findMin weights
        end = fst $ M.findMax weights
        solve1 = shortestPath (nearEdges weights (3,0)) (start, (0,1))
        res1 = min (solve1 (end, (1,0))) (solve1 (end, (0,1)))
        solve2 = shortestPath (nearEdges weights (7,3)) (start, (0,1))
        res2 = min (solve2 (end, (1,0))) (solve2 (end, (0,1)))
    print res1
    print res2
