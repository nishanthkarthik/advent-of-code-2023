{-# LANGUAGE OverloadedStrings #-}

import qualified Data.Attoparsec.Text as At
import qualified Data.Attoparsec.Combinator as Atc
import qualified Data.Text as T
import Control.Applicative
import Control.Monad
import Data.Functor

import AocLib

parser :: At.Parser [[Int]]
parser = map reverse <$> At.sepBy1 (At.sepBy1 (At.signed At.decimal) (At.many1 $ At.char ' ')) At.endOfLine

diff :: [Int] -> [Int]
diff as = zipWith (-) as (tail as)

buildTree :: [Int] -> [[Int]]
buildTree = takeWhile (any (/= 0)) . iterate diff

main :: IO ()
main = do
    rows <- parseInput parser
    let nextValue1 = sum . map head . buildTree
        solve1 = sum $ map nextValue1 rows
    print solve1

    let flippedRows = map reverse rows
        solve2 = sum $ map nextValue1 flippedRows
    print solve2
