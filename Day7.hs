{-# LANGUAGE OverloadedStrings #-}

import qualified Data.Attoparsec.Text as At
import qualified Data.Attoparsec.Combinator as Atc
import qualified Data.Text as T
import qualified Data.IntSet as IS
import Control.Applicative
import Control.Monad
import Data.Functor
import Data.Function
import Data.Bifunctor
import Data.List
import Data.Maybe (fromMaybe)

import qualified Data.Set as S
import qualified Data.Map.Lazy as M

import AocLib

data Card = C0 | C2 | C3 | C4 | C5 | C6 | C7 | C8 | C9 | CT | CJ | CQ | CK | CA deriving (Ord, Show, Eq)

newtype Hand = Hand { cards :: [Card] } deriving (Show, Eq) -- cards are sorted

data CType = HighCard | OnePair | TwoPair | ThreeKind | FullHouse | FourKind | FiveKind deriving (Show, Ord, Eq)

counter :: (Ord a, Eq a) => [a] -> M.Map a Int
counter as = M.fromList pairs
    where keys = S.toList $ S.fromList as
          count v = length $ filter (== v) as
          pairs = zip keys (map count keys)

cType :: Hand -> CType
cType (Hand cs)
    | S.size ks == 1 = FiveKind
    | S.size ks == 2 && vs == [1, 4] = FourKind
    | S.size ks == 2 && vs == [2, 3] = FullHouse
    | S.size ks == 3 && vs == [1, 1, 3] = ThreeKind
    | S.size ks == 3 && vs == [1, 2, 2] = TwoPair
    | S.size ks == 4 && vs == [1, 1, 1, 2] = OnePair
    | S.size ks == 5 = HighCard
    | otherwise = error "No match"
    where m = counter cs
          ks = M.keysSet m
          vs = sort $ map (m M.!) (M.keys m)

bumpCType :: Hand -> Hand
bumpCType (Hand cs) = if null nonJokerSuites then Hand cs else Hand newCards
    where m = counter cs
          nonJokerSuites = filter ((/= CJ) . fst) . M.assocs $ m
          (maxSuite, _) = maximumBy (compare `on` snd) nonJokerSuites
          njokers = fromMaybe 0 (m M.!? CJ)
          map2 = M.adjust (+ njokers) maxSuite (M.delete CJ m)
          newCards = concatMap (uncurry (flip replicate)) . M.assocs $ map2

weakenJoker :: Hand -> Hand
weakenJoker (Hand cs) = Hand (map (\i -> if i == CJ then C0 else i) cs)

compare1 :: Hand -> Hand -> Ordering
compare1 a b
    | a == b = EQ
    | cType a /= cType b = compare (cType a) (cType b)
    | otherwise = uncurry compare . head . filter (uncurry (/=)) $ zip (cards a) (cards b)

parseCard :: At.Parser Card
parseCard = At.choice [
        At.char '0' $> C0,
        At.char '2' $> C2,
        At.char '3' $> C3,
        At.char '4' $> C4,
        At.char '5' $> C5,
        At.char '6' $> C6,
        At.char '7' $> C7,
        At.char '8' $> C8,
        At.char '9' $> C9,
        At.char 'T' $> CT,
        At.char 'J' $> CJ,
        At.char 'Q' $> CQ,
        At.char 'K' $> CK,
        At.char 'A' $> CA
    ]

parser :: At.Parser [(Hand, Int)]
parser = flip At.sepBy1 At.endOfLine $ do
    cards <- Hand <$> At.count 5 parseCard <* At.skipSpace
    bet <- At.decimal
    return (cards, bet)

main :: IO ()
main = do
    input <- parseInput parser
    let sortedByStrength = sortBy (compare1 `on` fst) input
        solve = sum . zipWith (*) [1..]
        solve1 = solve $ map snd sortedByStrength
    print solve1

    let bumpedPairs = map (\(hand, bet) -> ((bumpCType hand, weakenJoker hand), bet)) input
        sortFn2 ((noJokerA, a), _) ((noJokerB, b), _)
            | cType noJokerA /= cType noJokerB = compare (cType noJokerA) (cType noJokerB)
            | otherwise = uncurry compare . head . filter (uncurry (/=)) $ zip (cards a) (cards b)
        sortedBumpedPairs = sortBy sortFn2 bumpedPairs
        solve2 = solve $ map snd sortedBumpedPairs
    print solve2
