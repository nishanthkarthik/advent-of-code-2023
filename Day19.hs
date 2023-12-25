{-# LANGUAGE OverloadedStrings #-}

import AocLib

import qualified Data.Attoparsec.Text as At
import qualified Data.Attoparsec.Combinator as Atc
import qualified Data.Text as T
import qualified Data.Set as S
import qualified Data.Map.Strict as M
import Data.Functor
import Data.Bifunctor
import Data.Char (isLower)
import Data.Maybe
import Data.List

import Debug.Trace

newtype Iden = Iden T.Text deriving (Eq, Show, Ord)

data Node = Named Iden | Accept | Reject deriving (Eq, Show)
data PartItem = Xp | Mp | Ap | Sp deriving (Eq, Show)
data Part = Part { xN :: Int, mN :: Int, aN :: Int, sN :: Int } deriving (Eq, Show)
data Rule = Default Node | Less PartItem Int Node | Greater PartItem Int Node deriving (Show)

type Workflows = M.Map Iden [Rule]

parser :: At.Parser (Workflows, [Part])
parser = do
    let comma = At.char ','
        eol = At.endOfLine
        iden = Iden . T.pack <$> At.many1 (At.satisfy isLower)
        target = At.choice [At.char 'A' $> Accept, At.char 'R' $> Reject, Named <$> iden]
        selector = At.choice [At.char 'x' $> Xp, At.char 'm' $> Mp, At.char 'a' $> Ap, At.char 's' $> Sp]
        rel op fn = fn <$> (selector <* At.char op) <*> (At.decimal <* At.char ':') <*> target
        rule = At.choice [rel '<' Less, rel '>' Greater, Default <$> target]
        rules = At.sepBy1 rule comma
        workflow = (,) <$> (iden <* At.char '{') <*> (rules <* At.char '}')
    ws <- M.fromList <$> At.sepBy1 workflow eol

    let comp = At.letter *> At.char '=' *> At.decimal <* At.option '_' comma
        comps = Part <$> comp <*> comp <*> comp <*> comp
    ps <- At.many' eol *> At.sepBy1 (At.char '{' *> comps <* At.char '}') eol
    return (ws, ps)

select :: PartItem -> Part -> Int
select Xp = xN
select Mp = mN
select Ap = aN
select Sp = sN

matchPart :: Part -> Rule -> Maybe Node
matchPart part (Less sel val tgt) = if select sel part < val then Just tgt else Nothing
matchPart part (Greater sel val tgt) = if select sel part > val then Just tgt else Nothing
matchPart part (Default tgt) = Just tgt

process :: Workflows -> Iden -> Part -> Node
process wfs wfid part = head $ mapMaybe (matchPart part) rules
    where rules = wfs M.! wfid

processToEnd :: Workflows -> Part -> Node -> (Part, Node)
processToEnd ws part Accept = (part, Accept)
processToEnd ws part Reject = (part, Reject)
processToEnd ws part (Named tgt) = processToEnd ws part (process ws tgt part)

negateRule :: Rule -> Rule
negateRule (Default n) = Default n
negateRule (Less part val node) = Greater part (val - 1) node
negateRule (Greater part val node) = Less part (val + 1) node

targetOfRule :: Rule -> Node
targetOfRule (Default n) = n
targetOfRule (Less _ _ n) = n
targetOfRule (Greater _ _ n) = n

collectRules :: Workflows -> Node -> [[Rule]]
collectRules ws start = catMaybes $ fn [] start
    where fn constraints Reject = [Nothing]
          fn constraints Accept = [Just constraints]
          fn constraints (Named cur) = let rules = ws M.! cur
                                           splitRule a = map (second head . (`splitAt` a)) [0..length a - 1]
                                           splits = map (first (map negateRule)) $ splitRule rules
                                           recurse = concatMap (\(rs,r) -> fn (r : reverse rs ++ constraints) (targetOfRule r))
                                       in recurse splits

-- all rules belong to the same selector
-- no default rules
solveConstraint :: [Rule] -> Integer
solveConstraint rules = max 0 (minLower - maxUpper - 1)
    where lowers = filter ((== 1) . key) rules
          minLower = minimum $ map val lowers
          uppers = filter ((== 2) . key) rules
          maxUpper = maximum $ map val uppers
          val :: Rule -> Integer
          val (Default _) = error "no value"
          val (Less _ v _) = fromIntegral v
          val (Greater _ v _) = fromIntegral v
          key :: Rule -> Integer
          key (Default _) = 0
          key (Less {}) = 1
          key (Greater {}) = 2

filterByPartItem :: PartItem -> Rule -> Maybe Rule
filterByPartItem _ a@(Default n) = Nothing
filterByPartItem p a@(Less p0 _ _) = if p == p0 then Just a else Nothing
filterByPartItem p a@(Greater p0 _ _) = if p == p0 then Just a else Nothing

main :: IO ()
main = do
    (ws, ps) <- parseInput parser
    let solve1 = sum $ map (score . fst) $ filter ((== Accept) . snd) $ map (flip (processToEnd ws) start) ps
        start = Named $ Iden "in"
        score (Part x m a s) = x + m + a + s
    print solve1
    let paths = map (++ defaultConstraints) $ collectRules ws start
        partItems = [Xp, Mp, Ap, Sp]
        defaultConstraints = concat [[Greater part 0 Accept, Less part 4001 Accept] | part <- partItems]
    let combs p = solveConstraint . mapMaybe (filterByPartItem p)
        solveRule r = product $ map (`combs` r) partItems
        solve2 = sum $ map solveRule paths
    print solve2
