import           RandState
import           State
import           System.Environment
import           System.IO
import           System.Random
import           Control.Monad

-- Data types to represent playing cards
data CardValue
    = King
    | Queen
    | Jack
    | NumberCard Int  -- From 1 to 10
    deriving (Show, Eq)

data CardSuit
    = Hearts
    | Diamonds
    | Spades
    | Clubs
    deriving (Show, Eq)

data PlayingCard =
    PlayingCard CardValue CardSuit
    deriving (Eq)

type Deck = [PlayingCard]


instance Show PlayingCard where
    show (PlayingCard value suit) =
        valueStr value ++ suitStr suit
        where
            suitStr Hearts   = "H"
            suitStr Diamonds = "D"
            suitStr Spades   = "S"
            suitStr Clubs    = "C"
            valueStr King           = "K"
            valueStr Queen          = "Q"
            valueStr Jack           = "J"
            valueStr (NumberCard n) = show n


-- fullCardDeck is a deck of cards, 52 in total, with a King, a Queen,
-- a Jack and NumberCards from 1 to 10 for each suit.
fullCardDeck :: Deck
fullCardDeck =
    [ PlayingCard v s | v <- allVals, s <- allSuits ]
    where
        allVals  = King : Queen : Jack : [ NumberCard i | i <- [1..10] ]
        allSuits = [Hearts, Diamonds, Spades, Clubs]

randR :: Random a => (a, a) -> RandState a
randR (low, high) = do
      gen <- get
      let (x, gen') = randomR (low, high) gen
      put gen'
      return x


rollTwoDice :: RandState Int
rollTwoDice = do
  first  <- randR (1, 6)
  second <- randR (1, 6)
  return (first + second)


shuffleDeck :: Deck -> RandState Deck
shuffleDeck as
  | length as == 0 = return []
  | length as == 1 = do
      (card, deck) <- removeCard as
      return [card]
  | length as == 2 = do
      (card, deck) <- removeCard as
      seconddeck <- shuffleDeck deck
      return $ (:) card seconddeck
  | otherwise = do
      (card, deck) <- removeCard as
      shuffleDeck deck
      return $ card : deck

shuffleADeck :: RandState Deck
shuffleADeck = shuffleDeck fullCardDeck

testshuffle :: RandState Deck
testshuffle = shuffleDeck [PlayingCard King Clubs, PlayingCard King Hearts, PlayingCard Queen Hearts, PlayingCard Jack Hearts]

removeCard :: Deck -> RandState (PlayingCard, Deck)
removeCard as = do
  index <- randR (0, length as - 1)
  let card = (as !! index)
      deck = (drop (index + 1) as ++ take index as)
  return (card, deck)


shuffleNTimes :: Int -> StdGen -> IO ()
shuffleNTimes nTimes gen = do
  let helper = replicateM nTimes shuffleADeck
  putStrLn $ unlines $ map show $ runRandom helper gen


rollTwoDiceNTimes :: Int -> StdGen -> IO ()
rollTwoDiceNTimes nTimes gen = do
  let helper = replicateM nTimes rollTwoDice
  putStrLn $ unlines $ map show $ runRandom helper gen


usage :: String
usage =
     "Lab 5: Randomizer\n" ++
     "\n" ++
     "$ ./Lab5 shuffle 600      # 600 times: output a full deck shuffle\n" ++
     "$ ./Lab5 rollTwoDice 800  # 800 times: output the sum of rolling two dice\n" ++
     "\n"

main :: IO ()
main = do
     gen  <- newStdGen
     args <- getArgs
     case args of
         ["shuffle",     nTimes] -> shuffleNTimes     (read nTimes) gen
         ["rollTwoDice", nTimes] -> rollTwoDiceNTimes (read nTimes) gen
         _                       -> putStrLn usage
