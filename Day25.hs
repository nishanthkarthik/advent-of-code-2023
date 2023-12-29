{-# LANGUAGE OverloadedStrings #-}

import AocLib

import qualified Data.Attoparsec.Text as At
import qualified Data.Attoparsec.Combinator as Atc
import qualified Data.Text as T
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.Sequence as Seq
import qualified Data.Array as A
import Data.Tuple (swap)
import Data.Bifunctor
import Data.List
import Control.Applicative

type Node = T.Text
type Graph = M.Map Node (S.Set Node)
parser :: At.Parser Graph
parser = do
    let node = T.pack <$> At.many1 At.letter
        row = zip . repeat <$> (node <* At.string ": ") <*> At.sepBy1 node (At.char ' ')
    rs <- concat <$> At.sepBy1 row At.endOfLine
    return $ edgesToGraph rs

edgesToGraph :: [(Node,Node)] -> Graph
edgesToGraph rs = M.fromListWith S.union $ map (second S.singleton) (rs ++ map swap rs)

graphToEdges :: Graph -> [(Node,Node)]
graphToEdges g = concatMap (\(k,v) -> zip (repeat k) $ S.toList v) $ M.toList g

mstEdges :: Graph -> S.Set Node -> S.Set Node -> [(Node,Node)]
mstEdges graph known unknown
    | S.null unknown = []
    | null edges = []
    | otherwise = newedges ++ mstEdges graph (S.union known to) (S.difference unknown to)
    where edges = filter (not . S.null . snd) $ map (\k -> (k, S.intersection unknown $ graph M.! k)) (S.toList known)
          (fr,to) = head edges
          newedges = zip (repeat fr) $ S.toList to

removeDuplicates :: [(Node,Node)] -> [(Node,Node)]
removeDuplicates xs = S.toList $ go S.empty xs
    where go kn xs
           | null xs = kn
           | otherwise = let cur = head xs
                             unknown = not (S.member cur kn) && not (S.member (swap cur) kn)
                             cur' = min cur (swap cur)
                         in go (if unknown then S.insert cur' kn else kn) $ tail xs

dfsSize :: Graph -> Node -> Int
dfsSize g st = go (S.singleton st) S.empty
    where go curs kn
           | S.isSubsetOf curs kn = S.size kn
           | otherwise = let nexts = map (\cur -> S.difference (g M.! cur) kn) $ S.toList curs
                         in go (foldr S.union S.empty nexts) (S.union kn curs)

main :: IO ()
main = do
    graph <- parseInput parser
    let startingNodes = filter (not . isPointedTo) $ M.keys graph
        isPointedTo n = any (S.member n) $ M.elems graph
        (kn,unkn) = S.splitAt 1 $ M.keysSet graph
    let redEdges = removeDuplicates $ mstEdges graph kn unkn
        others = removeDuplicates $ graphToEdges graph
    -- giving up and using the easy min-cut from neato on graphviz
    let soln = others \\ map (liftA2 min id swap) [("tbg","ljh"),("mnh","qnv"),("mfs","ffv")]
        g2 = edgesToGraph soln
    print $ dfsSize g2 "tbg"
    print $ dfsSize g2 "ljh"
    return ()
