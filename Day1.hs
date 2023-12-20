{-# LANGUAGE OverloadedStrings #-}

import qualified Data.Attoparsec.Text as At
import qualified Data.Attoparsec.Combinator as Atc
import qualified Data.Text as T
import Control.Applicative
import Data.Char (isDigit, digitToInt, intToDigit)
import Data.Functor
import Debug.Trace

import AocLib

parseTextDigit :: At.Parser Int
parseTextDigit = At.choice [
    At.string "one" >> return 1,
    At.string "two" >> return 2,
    At.string "three" >> return 3,
    At.string "four" >> return 4,
    At.string "five" >> return 5,
    At.string "six" >> return 6,
    At.string "seven" >> return 7,
    At.string "eight" >> return 8,
    At.string "nine" >> return 9]

parser1 :: At.Parser [[Int]]
parser1 = do
    let parseDigitDiscardingRest = (digitToInt <$> At.digit) <|> (At.letter *> parseDigitDiscardingRest)
    At.sepBy1 (At.many1 parseDigitDiscardingRest <* At.many' At.letter) At.endOfLine

parser2 :: At.Parser [[Int]]
parser2 = do
    let notEoL = At.notChar '\n'
        parseDigit = do
            c <- At.peekChar'
            if isDigit c
               then notEoL $> digitToInt c
               else do
                   (Atc.lookAhead parseTextDigit <* notEoL) <|> (notEoL *> parseDigit)
    At.many1 ((At.many1 parseDigit <* At.many' At.letter) <* At.endOfLine)

main :: IO ()
main = do
    let combine = (\a b -> 10 * a + b) <$> head <*> last
    input1 <- parseInput parser1
    input2 <- parseInput parser2
    let solve1 = sum $ map combine input1
        solve2 = sum $ map combine input2
    print (solve1, solve2)
