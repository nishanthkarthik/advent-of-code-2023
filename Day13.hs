{-# LANGUAGE OverloadedStrings #-}

import qualified Data.Attoparsec.Text as At
import qualified Data.Attoparsec.Combinator as Atc
import Control.Applicative
import Data.Functor
import Data.List
import Data.Int
import Data.Maybe
import Data.Bits

import AocLib

parser :: At.Parser [[[Int]]]
parser = flip At.sepBy1 (At.many1 At.endOfLine) $ do
    let item = At.choice [At.char '#' $> 1, At.char '.' $> 0]
        row = At.many1 item
        rows = At.sepBy1 row At.endOfLine
    rows

encodeGrid :: [[Int]] -> ([Int64], [Int64])
encodeGrid grid = (rows, cols)
    where rows = map encode grid
          cols = map encode (transpose grid)
          encode [] = 0
          encode (n:ns) = shiftL (encode ns) 1 .|. fromIntegral n

reflectionAt :: Int -> [Int64] -> Int -> Maybe Int
reflectionAt k xs i = let (l, r) = splitAt i xs
                          eq as bs = sum . map popCount $ zipWith xor as bs
                      in if eq (reverse l) r == k then Just i else Nothing

linesOfReflection :: Int -> [Int64] -> Int
linesOfReflection k xs = sum $ mapMaybe (reflectionAt k xs) [1 .. length xs - 1]

main :: IO ()
main = do
    input <- parseInput parser
    let grids = map encodeGrid input
        solve1 = liftA2 (+) ((100 *) . linesOfReflection 0 . fst) (linesOfReflection 0 . snd)
        solve2 = liftA2 (+) ((100 *) . linesOfReflection 1 . fst) (linesOfReflection 1 . snd)
    (print . sum . map solve1) grids
    (print . sum . map solve2) grids
    return ()