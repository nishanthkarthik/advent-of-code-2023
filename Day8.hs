{-# LANGUAGE OverloadedStrings #-}

import qualified Data.Attoparsec.Text as At
import qualified Data.Attoparsec.Combinator as Atc
import qualified Data.Text as T
import Control.Applicative
import Control.Monad
import Data.Functor
import Data.Bifunctor (bimap)
import Data.List
import qualified Data.Map.Lazy as M
import qualified Data.Set as S

import AocLib

data Dir = L | R deriving (Eq, Show)
type Table = M.Map T.Text (T.Text, T.Text)

parser :: At.Parser ([Dir], Table)
parser = do
    let parseDir = At.char 'R' $> R <|> At.char 'L' $> L
        parseId = T.pack <$> At.count 3 (At.letter <|> At.digit)
        parseEdge = do
            from <- parseId <* At.string " = ("
            lEdge <- parseId <* At.string ", "
            rEdge <- parseId <* At.string ")"
            return (from, (lEdge, rEdge))
    dirs <- At.many1 parseDir <* At.many' At.endOfLine
    edges <- At.sepBy1 parseEdge At.endOfLine
    return (dirs, M.fromList edges)

step1 :: Table -> T.Text -> Dir -> T.Text
step1 table cur dir = let (l, r) = table M.! cur
                      in case dir of
                              L -> l
                              R -> r

takeUntilRepeat :: S.Set (T.Text, Int) -> [(T.Text, Int)] -> [(T.Text, Int)]
takeUntilRepeat s xs
    | S.member (head xs) s = [head xs]
    | otherwise = head xs : takeUntilRepeat (S.insert (head xs) s) (tail xs)

main :: IO ()
main = do
    (dirs, table) <- parseInput parser
    let cycledDir = cycle dirs
        steps start = scanl (step1 table) start cycledDir
        solve1 = takeWhile (/= "ZZZ") (steps "AAA")
        indices = cycle [0..length dirs - 1]
    print (length solve1)
    let endingInA = filter ((== 'A') . T.last) $ M.keys table
        repeatedSteps start = zip (steps start) indices
        repeatedSeq = map fst . takeUntilRepeat S.empty . repeatedSteps
        loopChunk st = let items = repeatedSeq st
                           loopStartIdx = head $ elemIndices (last items) items
                           in drop loopStartIdx (init items)
    let loopLengths = map (length . loopChunk) endingInA
        lcms = foldl1 lcm loopLengths
    print lcms
