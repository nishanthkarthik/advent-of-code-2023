{-# LANGUAGE OverloadedStrings #-}

import AocLib

import qualified Data.Attoparsec.Text as At
import qualified Data.Attoparsec.Combinator as Atc
import qualified Data.Text as T
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.Sequence as Seq
import Data.Ix
import Data.Maybe
import Data.List
import Data.Function
import Data.Bifunctor
import Data.Containers.ListUtils (nubOrd)
import Text.Printf (printf)
import Control.Parallel.Strategies

type Idx = (Int,Int,Int)

type Cube = (Idx, Idx)

parser :: At.Parser [Cube]
parser = do
    let idx = (,,) <$> (At.decimal <* sep) <*> (At.decimal <* sep) <*> At.decimal
        sep = At.char ','
        cube = (,) <$> idx <*> (At.char '~' *> idx)
    At.sepBy1 cube At.endOfLine

add :: Idx -> Idx -> Idx
add (x,y,z) (a,b,c) = (x+a,y+b,z+c)

lowerPlane :: Cube -> [Idx]
lowerPlane ((x1,y1,z1),(x2,y2,z2)) = [(x,y,min z1 z2) | x <- [min x1 x2..max x1 x2], y <- [min y1 y2..max y1 y2]]

supportsOf :: [Cube] -> Cube -> S.Set Cube
supportsOf cubes this = supports
    where lowerBlocks = map (add (0,0,-1)) $ lowerPlane this
          blockInOtherCubes bl = filter (\c@(a,b) -> c /= this && inRange (min a b, max a b) bl)
          supports = S.fromList $ concatMap (`blockInOtherCubes` cubes) lowerBlocks

fallBlockOnCube :: Idx -> Cube -> Maybe Int
fallBlockOnCube (x,y,z) ((x1,y1,z1),(x2,y2,z2)) = if overlap then Just (max 0 delta) else Nothing
    where a = min (x1,y1) (x2,y2)
          b = max (x1,y1) (x2,y2)
          overlap = inRange (a,b) (x,y)
          delta = z - max z1 z2 - 1

settle :: [Cube] -> [Cube]
settle cubes = go cubes []
    where go [] settled = settled
          go (this:rest) settled = go rest (offsetByDelta this : settled)
            where decz z = add (0,0,negate z)
                  dists = map (\bl -> mapMaybe (fallBlockOnCube bl) settled) $ lowerPlane this
                  fallDelta
                      | (not . all null) dists = minimum $ concat dists
                      | otherwise = let ((_,_,z1),(_,_,z2)) = this in min z1 z2 - 1
                  offsetByDelta = bimap (decz fallDelta) (decz fallDelta)

bfs :: M.Map Cube (S.Set Cube) -> M.Map Cube (S.Set Cube) -> Cube -> Int
bfs supportedBy supports start = run (S.singleton start) (Seq.fromList $ S.toList $ M.findWithDefault S.empty start supports)
    where run :: S.Set Cube -> Seq.Seq Cube -> Int
          run deleted tovisit
              | Seq.null tovisit = S.size deleted - 1
              | otherwise = let (cur Seq.:<| rest) = tovisit
                                allParentsDeleted c = S.isSubsetOf (M.findWithDefault S.empty c supportedBy) deleted
                                notDeleted c = not $ S.member c deleted
                                nexts = S.toList $ M.findWithDefault S.empty cur supports
                                negZ ((x1,y1,z1),(x2,y2,z2)) = negate $ min z1 z2
                                tovisit' = rest Seq.>< Seq.fromList (sortOn negZ $ filter notDeleted nexts)
                            in if allParentsDeleted cur
                                  then run (S.insert cur deleted) tovisit'
                                  else run deleted rest

main :: IO ()
main = do
    cubes <- parseInput parser
    let sortCubesByZ = sortOn (\((_,_,z1),(_,_,z2)) -> (min z1 z2, max z1 z2))
        settled = settle $ sortCubesByZ cubes
        supports = parMap rpar (supportsOf settled) settled
    let standaloneSupports = foldr S.union S.empty $ filter ((== 1) . S.size) supports
        solve1 = length supports - S.size standaloneSupports
    print solve1
    --putStrLn (unlines $ map (\((x1,y1,z1),(x2,y2,z2)) -> printf "range(%d,%d,%d,%d,%d,%d);" x1 y1 z1 x2 y2 z2) settled)

    let supportMap = M.fromListWith S.union $ concat $ zipWith (\a s -> zip (S.toList s) (repeat (S.singleton a))) settled supports
        supportedByMap = M.fromList $ zip settled supports
        start = last settled
        n = length settled
        dfsSizes = parMap rpar (bfs supportedByMap supportMap) $ S.toList standaloneSupports
        solve2 = dfsSizes
    -- print solve2
    print $ sum solve2
