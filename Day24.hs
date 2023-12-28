{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}

import AocLib

import qualified Data.Attoparsec.Text as At
import qualified Data.Attoparsec.Combinator as Atc
import qualified Data.Text as T
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.Sequence as Seq
import qualified Data.Array as A

import Data.Bifunctor
import Data.Ix
import Data.Maybe
import Data.List
import Data.Ratio
import Data.Containers.ListUtils (nubOrd)

data V3 = V3 Integer Integer Integer deriving (Eq, Show)

type Position = V3
type Velocity = V3
type State = (Position, Velocity)

type V4 = (Integer, Integer, Integer, Integer)

solve2 :: V4 -> V4 -> Maybe (Rational,Rational)
solve2 (a,b,c,d) (m,n,p,q)
    | d * n == b * q = Nothing
    | otherwise = Just (t,s)
    where t = (a * n + b * p - b * m - c * n) % (d * n - b * q)
          s = (c * q - p * d + m * d - a * q) % (b * q - d * n)

type R3 = (Rational, Rational, Rational)

solve3 :: (V3,V3) -> (V3,V3) -> Maybe R3
solve3 (V3 x1 y1 z1, V3 vx1 vy1 vz1) (p2@(V3 x2 y2 z2), v2@(V3 vx2 vy2 vz2))
    | null solns = Nothing
    | not unique = Nothing
    | t < 0 || s < 0 = Nothing
    | otherwise = Just (fromIntegral x2 + t * fromIntegral vx2, fromIntegral y2 + t * fromIntegral vy2, fromIntegral z2 + t * fromIntegral vz2)
    where axes = [(x1,vx1,x2,vx2),(y1,vy1,y2,vy2),(z1,vz1,z2,vz2)]
          pairs = zip axes (tail $ cycle axes)
          solns = mapMaybe (uncurry solve2) pairs
          unique = 1 == length (nubOrd solns)
          (t,s) = head solns

parser :: At.Parser [State]
parser = do
    let n = At.signed At.decimal <* At.option "" sep
        skip c = c `elem` (", @" :: String)
        sep = At.many1 $ At.satisfy skip
        v3 = V3 <$> n <*> n <*> n
        pair = (,) <$> v3 <*> v3
    At.sepBy1 pair At.endOfLine

intersect2D :: State -> State -> Maybe R3
intersect2D (V3 x1 y1 _, V3 vx1 vy1 _) (V3 x2 y2 _, V3 vx2 vy2 _)
    | isJust res && t > 0 && s > 0 = Just (fromIntegral x2 + t * fromIntegral vx2, fromIntegral y2 + t * fromIntegral vy2, 0)
    | otherwise = Nothing
    where res = solve2 (x1,vx1,x2,vx2) (y1,vy1,y2,vy2)
          Just (t,s) = res

type Eqn = ([Rational], Rational)

makeLinEq :: ((Rational, Rational), (Rational, Rational)) -> ((Rational, Rational), (Rational, Rational)) -> Eqn
makeLinEq ((x1,y1),(vx1,vy1)) ((x2,y2),(vx2,vy2)) = ([x0c, y0c, vx0c, vy0c], c)
    where x0c = vy1 - vy2
          y0c = vx2 - vx1
          vx0c = y2 - y1
          vy0c = x1 - x2
          c = vx2 * y2 - vx1 * y1 + x1 * vy1 - x2 * vy2

makeEqns :: [State] -> ([Eqn],[Eqn])
makeEqns ss = bimap uniquePairs uniquePairs $ unzip xypairs
    where xypairs = [(makeXYEqns s1 s2, makeYZEqns s1 s2) | s1:bs <- tails ss, s2 <- bs]
          vToXY (V3 x y z) = (fromIntegral x, fromIntegral y)
          makeXYEqns (p1,v1) (p2,v2) = makeLinEq (vToXY p1, vToXY v1) (vToXY p2, vToXY v2)
          vToYZ (V3 x y z) = (fromIntegral y, fromIntegral z)
          makeYZEqns (p1,v1) (p2,v2) = makeLinEq (vToYZ p1, vToYZ v1) (vToYZ p2, vToYZ v2)
          ratio a b
              | a == b = Just 1
              | a == 0 || b == 0 = Nothing
              | otherwise = Just (a / b)
          isSame (as,ac) (bs,bc) = unique $ zipWith ratio (ac : as) (bc : bs)
          unique = (== 1) . length . nubOrd
          uniquePairs xs = filter (\x -> any (isSame x) xs) xs

offsetEqn :: Eqn -> Eqn -> Eqn
offsetEqn (ref,refc) (a,c) = (zipWith (\i j -> i - r * j) a ref, c - r * refc)
    where r = head a / head ref

rowEchelon :: [Eqn] -> [Eqn]
rowEchelon [] = []
rowEchelon es
    | null valid = head es : rec (tail es)
    | otherwise = selected : rec otherEqns
    where valid = filter ((/= 0) . head . fst) es
          selected = head valid
          otherEqns = map (offsetEqn selected) . (\\ [selected]) $ es
          rec = rowEchelon . map (first tail)

solveEchelon :: [Eqn] -> [Rational]
solveEchelon es = go es []
    where go :: [Eqn] -> [Rational] -> [Rational]
          go [] r = r
          go ((as,ac) : rest) vals = go rest (((ac - sum (zipWith (*) vals (tail as))) `relaxedDiv` head as) : vals)
          relaxedDiv a b = if a == b then 1 else a / b

solveEqn :: [Eqn] -> [Rational]
solveEqn = solveEchelon . reverse . rowEchelon

main :: IO ()
main = do
    pairs <- parseInput parser
    let ll = 200000000000000
        ul = 400000000000000

    let xions2 = catMaybes [intersect2D a b | (a:bs) <- tails pairs, b <- bs]
        valid a = ll < a && a < ul
        within = filter (\(x,y,z) -> valid x && valid y) xions2
    print $ length within

    let (xys,yzs) = makeEqns $ take 5 pairs
    let xysoln = solveEqn $ take 4 xys
        yzsoln = solveEqn $ take 4 yzs
        px = head xysoln
        py = head yzsoln
        pz = yzsoln !! 1
    print (px + py + pz)
