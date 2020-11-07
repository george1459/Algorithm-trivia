-- Write your parser in this file.

module Lab6 (
  Name,
  Number,
  TopLevelExp(..),
  MathExp(..),
  parse
) where

import           Control.Applicative          hiding (many)
import           Control.Monad
import           Data.Char
import           Data.List
import           Data.Ord
import           Text.ParserCombinators.ReadP

type Name   = String  -- Variable names are strings.
type Number = Int     -- The kind of number in our language.

data TopLevelExp
    = MathTLE MathExp
    | LetTLE [Name] [MathExp] MathExp
    deriving (Eq, Show)

data MathExp
    = Number Number
    | Var    Name
    | Neg    MathExp
    | Plus   MathExp MathExp
    | Minus  MathExp MathExp
    | Mult   MathExp MathExp
    | Div    MathExp MathExp
    | Pow    MathExp MathExp
    deriving (Eq, Show)

parseNumber :: ReadP MathExp
parseNumber = do
  skipSpaces
  x <- munch1 isNumber
  return $ Number . read $ x

-- built to recognize names like a2B5
newpredicate :: Char -> Bool
newpredicate c = if isAlpha c || isNumber c == True then True else False

parseName :: ReadP MathExp
parseName = do
  skipSpaces
  x <- munch1 isAlpha
  y <- munch newpredicate
  return $ Var (x ++ y)

parseOperator :: Char -> ReadP (MathExp -> MathExp -> MathExp)
parseOperator operator = do
  skipSpaces
  satisfy (== operator)
  return $ transfer operator

transfer operator
  | operator == '+' = Plus
  | operator == '-' = Minus
  | operator == '*' = Mult
  | operator == '/' = Div
  | operator == '^' = Pow

parsePlus  = parseOperator '+'
parseMinus = parseOperator '-'
parseMult  = parseOperator '*'
parseDiv   = parseOperator '/'
parsePow   = parseOperator '^'

parseNeg :: ReadP MathExp
parseNeg = do
  skipSpaces
  satisfy (== '-')
  exps <- parseNewPow <++ parseParethese <++ (parseNumber <|> parseName)
  return $ Neg exps

parseNewPow :: ReadP MathExp
parseNewPow = chainr1 (parseParethese <++ (parseNumber <|> parseName)) parsePow

parseParethese :: ReadP MathExp
parseParethese = do
  skipSpaces
  satisfy (== '(')
  exps <- parseMath
  skipSpaces
  satisfy (== ')')
  return exps

parseMath :: ReadP MathExp
parseMath = do
  lowestLevel where
  highestLevel = parseNeg <++ parseNewPow <++ parseParethese <++ (parseNumber <|> parseName)
  nextLevel   = chainl1 highestLevel (parseMult <|> parseDiv)
  lowestLevel  = chainl1 nextLevel (parsePlus <|> parseMinus)

parseMathTLE :: ReadP TopLevelExp
parseMathTLE = MathTLE <$> parseMath

---------------------------------------------------------------------------------------------------------------------------
parseLetTLE :: ReadP TopLevelExp
parseLetTLE = do
    skipSpaces
    string "let"
    names <- munch1 (/= '=')
    satisfy (== '=')
    values <- munch1 (/= 'i')
    string "in"
    exps <- parseMath
    let noSpacesNames  = chopoffNames $ dropLastAndFilter names
        noSpacesValues = map (fst . last) $ map (readP_to_S parseMath) (chopoffNames $ dropLastAndFilter values)
    return $ LetTLE noSpacesNames noSpacesValues exps

-- first filter out all spaces, then get rid of the last parathese element if the let is built with tuples
dropLastAndFilter :: String -> String
dropLastAndFilter ss
  | elem '(' ss == True = drop 1 $ take (length (filter (/= ' ') ss) - 1) $ filter (/= ' ') ss
  | otherwise           = take (length (filter (/= ' ') ss)) $ filter (/= ' ') ss

-- chopoffNames and chophelper together transfer "1,2,3" into ["1","2","3"]
chopoffNames :: String -> [Name]
chopoffNames ss =
  drop 1 $ chophelper (',' : ss) (findIndices (== ',') (',' : ss))

chophelper :: String -> [Int] -> [String]
chophelper ss [] = [ss]
chophelper ss (i : is) = (take i ss) : chophelper (drop (i+1) ss) (map (+ (-i - 1)) is)

------------------------------------------------------------------------------------------------------------------------------
parseTLE :: ReadP TopLevelExp
parseTLE = do
    tle <- parseLetTLE +++ parseMathTLE
    skipSpaces
    return tle

-- Run the parser on a given string.
--
-- You should not modify this function. Grading may
-- look for the specific messages below.
parse :: String -> Either String TopLevelExp
parse str =
    case (completeParses, incompleteParses) of
        ([(result, "")], _  ) -> Right result  -- Only complete result.
        ([]            , [] ) -> Left $ "No parse."
        ([]            , _:_) -> Left $ "Incomplete parse. Unparsed: " ++ show leastRemaining
        (_:_           , _  ) -> Left $ "Ambiguous parse: " ++ show completeParses
    where
        parses = readP_to_S parseTLE str
        (completeParses, incompleteParses) =
            partition (\(_, remaining) -> remaining == "") parses
        leastRemaining = minimumBy (comparing length) . map snd $ incompleteParses
