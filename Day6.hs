{-# LANGUAGE OverloadedStrings #-}

import qualified Data.Attoparsec.Text as At
import qualified Data.Attoparsec.Combinator as Atc
import qualified Data.Text as T
import qualified Data.IntSet as IS
import Control.Applicative
import Control.Monad

import AocLib

parser :: At.Parser ([Int], [Int])
parser = do
    let nums = spaces *> At.sepBy1 At.decimal spaces
        spaces = At.many1 (At.char ' ')
    times <- At.string "Time:" *> nums <* At.endOfLine
    distance <- At.string "Distance:" *> nums
    return (times, distance)

mergeNums :: [Int] -> Int
mergeNums = foldl1 (\ac cu -> ac * 10 ^ digitCount cu + cu)
    where digitCount :: Int -> Int
          digitCount = ceiling . logBase 10 . fromIntegral

main :: IO ()
main = do
    (ts, ds) <- parseInput parser
    let pairs = zip (map fromIntegral ts) (map fromIntegral ds)
        root1 t d = (t + sqrt (t * t - 4 * d)) / 2
        root2 t d = (t - sqrt (t * t - 4 * d)) / 2
        nonIncCeil a = ceiling a + if fromIntegral (ceiling a) == a then 1 else 0
        nonIncFloor a = floor a - if fromIntegral (floor a) == a then 1 else 0
        solve1 t d = let r1 = root1 t d
                         r2 = root2 t d in
            1 + nonIncFloor (max r1 r2) - nonIncCeil (min r1 r2)
    print (product $ map (uncurry solve1) pairs)

    let t2 = mergeNums ts
        d2 = mergeNums ds
    print (solve1 (fromIntegral t2) (fromIntegral d2))
