{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

import AocLib

import qualified Data.Attoparsec.Text as At
import qualified Data.Attoparsec.Combinator as Atc
import qualified Data.Text as T
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import Data.Sequence (Seq, Seq((:<|)), Seq(Empty), fromList, singleton, (><))
import Data.Bifunctor
import Data.List (unfoldr)
import Control.Applicative

import Debug.Trace

newtype Iden = Iden T.Text deriving (Eq, Ord)

instance Show Iden where
    show (Iden t) = show t

type State = Bool

data Cell = Tap [State] | CeilTap State | FlipFlop State | Nand (M.Map Iden State) | Wire deriving (Eq, Show, Ord)

type Store = M.Map Iden Cell
type Connection = M.Map Iden [Iden]

type QueueItem = (Iden, Iden, State)
type Queue = Seq QueueItem

parser :: At.Parser (Store, Connection)
parser = do
    let iden = Iden . T.pack <$> At.many1 At.letter
        flfl = (,) <$> (At.char '%' *> iden ) <*> pure (FlipFlop False)
        nand = (,) <$> (At.char '&' *> iden) <*> pure (Nand M.empty)
        tap = (,) <$> (At.char '+' *> iden) <*> pure (Tap [])
        ceiltap = (,) <$> (At.char '^' *> iden) <*> pure (CeilTap False)
        wire = (,) <$> iden <*> pure Wire
        cell = At.choice [flfl, nand, tap, wire, ceiltap]
        row = (,) <$> cell <*> At.option [] (At.string " -> " *> At.sepBy1 iden (At.string ", "))
    rs <- At.sepBy1 row At.endOfLine
    let store = M.fromList $ map fst rs
        conns = M.fromList $ map (first fst) rs
        isNand (Nand {}) = True
        isNand _ = False
        initNandState = liftA2 (,) fst (Nand . M.fromList . map (,False) . incoming . fst)
        nands = map initNandState $ filter (isNand . snd) $ M.toList store
        incoming x = map fst . filter ((x `elem`) . snd) $ M.toList conns
        store' = foldr (uncurry M.insert) store nands

    return (store', conns)

connLookup :: Connection -> Iden -> [Iden]
connLookup m k = M.findWithDefault [] k m

storeLookup :: Store -> Iden -> Cell
storeLookup m k = M.findWithDefault Wire k m

process :: Connection -> QueueItem -> Cell -> (Cell, [QueueItem])
process con (from, cur, st) (Tap sts) = (Tap (st : sts), [])
process con (from, cur, st) (CeilTap sts) = (CeilTap (st || sts), [])
process con (from, cur, st) (FlipFlop fl)
    | not st = (FlipFlop $ not fl, map (cur,,not fl) (connLookup con cur))
    | otherwise = (FlipFlop fl, [])
process con (from, cur, st) (Nand stm) = (Nand stm', pulses)
    where stm' = M.insert from st stm
          allHigh = and $ M.elems stm'
          pulses = map (cur,,not allHigh) (connLookup con cur)
process con (from, cur, st) Wire = (Wire, map (cur,,st) (connLookup con cur))

simulate1 :: Connection -> Queue -> Store -> ((Int, Int), Store)
simulate1 conn Empty store = ((0,0), store)
simulate1 conn (x@(fr,cur,st) :<| xs) store = let (cell', sigs) = process conn x $ storeLookup store cur
                                                  q' = (xs >< fromList sigs)
                                                  ((lc,hc),st') = simulate1 conn q' (M.insert cur cell' store)
                                                  (dl,dh) = case storeLookup store cur of
                                                                 Tap _ -> (0,0)
                                                                 CeilTap _ -> (0,0)
                                                                 _ -> if st then (0,1) else (1,0)
                                              in ((lc+dl, hc+dh), st')

main :: IO ()
main = do
    (store, conn) <- parseInput parser
    let trigger = simulate1 conn (singleton (Iden "broadcaster", Iden "broadcaster", False))
        stream1 = take 1000 $ unfoldr (Just . trigger) store
        solve1 = liftA2 (*) (sum . map fst) (sum . map snd) stream1
    print solve1

    -- manually insert some taps into the input file
    let step2 = iterate (snd . trigger) store
        dfins = let Nand nm = store M.! Iden "df" in M.keys nm
        taps = map (\(Iden i) -> Iden (T.append i "tap")) dfins
        nToHigh t = length $ takeWhile (\st -> let CeilTap x = st M.! t in not x) step2
    print $ product $ map nToHigh taps
    return ()
