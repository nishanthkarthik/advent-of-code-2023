{-# LANGUAGE OverloadedStrings #-}

import qualified Data.Attoparsec.Text as At
import qualified Data.Attoparsec.Combinator as Atc
import qualified Data.Set as S
import qualified Data.IntMap.Strict as IM
import Control.Monad
import Control.Applicative
import Data.Char (ord)

import AocLib

parser1 :: At.Parser Int
parser1 = sum <$> At.sepBy1 (hash <$> At.many1 (At.satisfy $ At.inClass "-a-z=0-9")) (At.char ',')

hash :: String -> Hash
hash = foldl (\ac cu -> 17 * (ac + ord cu) `mod` 256) 0

type Hash = Int
data Op = Upsert String Hash Int | Remove String Hash deriving (Eq, Show)

parser2 :: At.Parser [Op]
parser2 = do
    let iden = At.many1 At.letter
        upsert = (,) <$> (iden <* At.char '=') <*> At.decimal
        upsertOp (iden, n) = Upsert iden (hash iden) n
        remove = iden <* At.char '-'
        removeOp iden = Remove iden (hash iden)
    At.sepBy1 (upsertOp <$> upsert <|> removeOp <$> remove) (At.char ',')

type Box = IM.IntMap [(String, Int)]

applyOp :: Box -> Op -> Box
applyOp box (Remove iden code) = IM.adjust (filter ((/= iden) . fst)) code box
applyOp box (Upsert iden code val)
    | not (IM.member code box) || all ((/= iden) . fst) (box IM.! code) = IM.adjust ((iden, val) :) code slottedBox
    | otherwise = IM.adjust (map (\(i, v) -> (i, if i == iden then val else v))) code box
    where slottedBox = if IM.member code box then box else IM.insert code [] box

focusingPower :: Box -> Int
focusingPower box = sum $ map score as
    where as = IM.toList box
          score (k,vs) = sum $ zipWith (\i (_,fl) -> succ k * i * fl) [1..] $ reverse vs

main :: IO ()
main = do
    solve1 <- parseInput parser1
    print solve1
    in2 <- parseInput parser2
    let solve2 = focusingPower $ foldl applyOp IM.empty in2
    print solve2
