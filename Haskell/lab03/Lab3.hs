module Lab3 where

import Data.List
import Data.Char
import Debug.Trace
import Control.Applicative


data ArithExp = Number Int
              | Plus ArithExp ArithExp
              | Mult ArithExp ArithExp
              | Div ArithExp ArithExp
    deriving (Show)


eval :: ArithExp -> Int
eval (Number a) = a
eval (Plus (Number a) (Number b)) = a + b
eval (Mult (Number a) (Number b)) = a * b
eval (Div (Number a) (Number b))  = a `quot` b
eval (Plus b c)                   = eval (Plus (Number (eval b)) (Number (eval c)))
eval (Mult b c)                   = eval (Mult (Number (eval b)) (Number (eval c)))
eval (Div b c)                    = eval (Div (Number (eval b)) (Number (eval c)))


data Token = TokenNum [Char]
           | PlusTok
           | MultTok
           | DivTok
           | LeftP
           | RightP
           | Para [Token]
    deriving (Show, Eq)



tokenize :: [Char] -> [Token]
tokenize a =
 case findIndex isSpace a of
    Just b  -> sanitize (take b a) ++ tokenize (drop (b + 1) a)
    Nothing -> sanitize a


{- "1+(2+(3+4)+(5+6))"  -}


paraTokenize :: [Token] -> [Token]
paraTokenize a =
  case findIndex (== LeftP) a of
    Just lpara  -> take lpara a ++ [Para $ paraTokenize ((take (right - (lpara + 1)) $ drop (lpara + 1) a))] ++ (paraTokenize $ drop (right + 1) a) where
      right = head $ filter isValid (findIndices (== RightP) a) where
        isValid :: Int -> Bool
        isValid i = (length $ filter (== LeftP) (drop (lpara + 1) $ take i a)) == (length $ filter (== RightP) (drop (lpara + 1) $ take i a))
    Nothing -> a



transfering :: [Char] -> [Token]
transfering [] = []
transfering (a:as)
  | isDigit a == True = TokenNum [a] : transfering as
  | a == '-'          = TokenNum "-" : transfering as
  | a == '+'          = PlusTok : transfering as
  | a == '*'          = MultTok : transfering as
  | a == '/'          = DivTok  : transfering as
  | a == '('          = LeftP   : transfering as
  | a == ')'          = RightP  : transfering as


sanitizing :: [Token] -> [Token]
sanitizing [] = []
sanitizing (TokenNum a : as) =
  case as of
    (TokenNum b : bs) -> sanitizing (TokenNum (concat $ a : b : []) : bs)
    (_ : bs) -> TokenNum a : sanitizing as
    []       -> TokenNum a : []
sanitizing (PlusTok : as)    = PlusTok : sanitizing as
sanitizing (MultTok : as)    = MultTok : sanitizing as
sanitizing (DivTok : as)     = DivTok : sanitizing as
sanitizing (LeftP : as)      = LeftP : sanitizing as
sanitizing (RightP : as)     = RightP : sanitizing as


sanitize :: [Char] -> [Token]
sanitize = sanitizing . transfering




-- parse :: [Token] -> ArithExp

readParse :: Token -> ArithExp
readParse (TokenNum a) = Number (read a :: Int)

parse :: [Token] -> ArithExp
parse [a] = readParse a

-- core function
core :: [Token] -> ArithExp
core [Para s] = core s
core a
 | elem PlusTok a == True =
    case findIndex (== PlusTok) (reverse a) of
      Just b  -> Plus (core $ fst $ splitAt (length a - b - 1) a) (core $ drop 1 $ snd $ splitAt (length a - b - 1) a)
 | elem MultTok a || elem DivTok a == True =
    case minM (findIndex (== MultTok) (reverse a)) (findIndex (== DivTok) (reverse a)) of
      Just c  -> if findIndex (== MultTok) (reverse a) == Just c then Mult (core $ fst $ splitAt (length a - c - 1) a) (core $ drop 1 $ snd $ splitAt (length a - c - 1) a)
                                                                 else Div  (core $ fst $ splitAt (length a - c - 1) a) (core $ drop 1 $ snd $ splitAt (length a - c - 1) a)
 | otherwise   =  parse a



minM :: Maybe Int -> Maybe Int -> Maybe Int
minM (Just a) (Just b) = Just (min a b)
minM (Just a) Nothing  = Just a
minM  Nothing (Just b) = Just b
minM Nothing Nothing   = Nothing
