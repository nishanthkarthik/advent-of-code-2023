{-# LANGUAGE OverloadedStrings #-}

import qualified Data.Attoparsec.Text as At
import qualified Data.Attoparsec.Combinator as Atc
import qualified Data.Text as T
import qualified Data.Set as S
import Control.Applicative
import Control.Monad
import Data.Functor
import Data.List
import Data.Bifunctor
import Control.Parallel.Strategies
import Data.Array

import Debug.Trace

import AocLib

parser :: At.Parser [([Char], [Int])]
parser = do
    let springs = At.many1 (At.satisfy (At.inClass ".#?"))
        nums = At.sepBy1 At.decimal (At.char ',')
    At.sepBy1 (liftA2 (,) springs (At.space *> nums)) At.endOfLine

-- too slow
subst2 :: Char -> (Int, Int, Int, Int) -> String -> [Int] -> Int
subst2 _ (lx,ln,sn,un) [] [] = 1
subst2 p (lx,ln,sn,un) ('.':xs) [] = subst2 '.' (lx-1,ln,sn,un) xs []
subst2 p (lx,ln,sn,un) ('?':xs) [] = subst2 '.' (lx-1,ln,sn,un-1) xs []
subst2 p (lx,ln,sn,un) ('#':xs) [] = 0
subst2 p (lx,ln,sn,un) str [] = error "unreachable"
subst2 p (lx,ln,sn,un) [] [0] = 1
subst2 p (lx,ln,sn,un) [] (n:ns) = 0
subst2 p (lx,ln,sn,un) (x:xs) (n:ns)
    | lx < (sn + (if n == 0 then pred ln else ln) - 1) = 0 -- early prune
    | un < sn = 0 -- need enough # or ?
    | n == 0 && x == '#' = 0
    | n == 0 && x == '.' = subst2 '.' (lx-1,ln-1,sn,un) xs ns
    | n == 0 && x == '?' = subst2 '.' (lx-1,ln-1,sn,un-1) xs ns
    | n > 0 && x == '#' = subst2 '#' (lx-1,ln,sn-1,un-1) xs (n-1:ns)
    | n > 0 && p == '.' && x == '.' = subst2 '.' (lx-1,ln,sn,un) xs (n:ns)
    | n > 0 && p == '.' && x == '?' = subst2 '#' (lx-1,ln,sn-1,un-1) xs (n-1:ns) + subst2 '.' (lx-1,ln,sn,un-1) xs (n:ns)
    | n > 0 && p == '#' && x == '.' = 0
    | n > 0 && p == '#' && x == '?' = subst2 '#' (lx-1,ln,sn-1,un-1) xs (n-1:ns)
    | otherwise = 0

subst :: Char -> String -> [Int] -> Int
subst c xs ns = subst2 c (length xs, length ns, sum ns, length (filter (\i -> i == '#' || i == '?') xs)) xs ns

-- https://www.reddit.com/r/adventofcode/comments/18ge41g/2023_day_12_solutions/kd18cl9/
solve' :: String -> [Bool] -> Int
solve' xs bs = arr ! (0, 0)
    where arr = array bound $ ((lx,lb), 1) : nullCells ++ [((i,j), fn i j) | i <- [0..lx-1], j <- [0..lb-1]]
          nullCells = [((lx,j), 0) | j <- [0..lb-1]] ++ [((i,lb),0) | i <- [0..lx-1]]
          bound@(_,(lx,lb)) = ((0, 0), (length xs, length bs))
          fn i j = g i j (xs !! i) (bs !! j)
          g i j '#' True = arr ! (i+1,j+1)
          g i j '.' False = arr ! (i+1,j+1) + arr ! (i+1,j)
          g i j '#' False = 0
          g i j '.' True = 0
          g i j '?' b = g i j '.' b + g i j '#' b
          g _ _ _ _ = error "unreachable"

main :: IO ()
main = do
    rows <- parseInput parser
    let step xs ns = solve' ('.' : xs ++ ".") (numsToBools ns)
        numsToBools ns = concatMap (\i -> False : replicate i True) ns ++ [False]
        solve = sum . map (uncurry step)
    print (solve rows)
    let repeat5Str = intercalate "?" . replicate 5
        repeat5Num = concat . replicate 5
        input2 = map (bimap repeat5Str repeat5Num) rows
    print (solve input2)
    return ()