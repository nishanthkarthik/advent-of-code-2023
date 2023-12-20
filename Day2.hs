{-# LANGUAGE OverloadedStrings #-}

import qualified Data.Attoparsec.Text as At
import qualified Data.Attoparsec.Combinator as Atc
import qualified Data.Text as T
import Control.Applicative
import Data.Functor
import Data.List (lookup, transpose)
import Data.Maybe (fromMaybe)
import AocLib

data Color = Red | Green | Blue deriving (Show, Eq)
type Draw = [Int]
data Game = Game { gid :: Int, draws :: [Draw] } deriving (Show)

parseDraw :: At.Parser Draw
parseDraw = do
    let parseDrawItem = do
            n <- At.decimal <* At.space
            c <- parseColor
            return (c, n)
        parseDrawItems = At.sepBy1 parseDrawItem (At.string ", ")
        parseColor = At.choice [At.string "red" $> Red,
                                At.string "green" $> Green,
                                At.string "blue" $> Blue]
    items <- parseDrawItems
    let r c = fromMaybe 0 $ lookup c items
    return [r Red, r Green, r Blue]

parser :: At.Parser [Game]
parser = flip At.sepBy1 At.endOfLine $ do
    gameId <- At.string "Game " *> At.decimal <* At.string ": "
    draws <- At.sepBy1 parseDraw (At.string "; ")
    return (Game gameId draws)

main :: IO ()
main = do
    input <- parseInput parser
    let bagSize = [12, 13, 14]
        validDraw as = all (>= 0) (zipWith (-) bagSize as)
        validGames = filter (\(Game gid draws) -> all validDraw draws) input
    print (sum $ map gid validGames)
    let minCubes draws = map maximum $ transpose draws
        power game = product $ minCubes $ draws game
    print (sum $ map power input)
