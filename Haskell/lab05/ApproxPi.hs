import           RandState
import           State
import           System.Environment
import           System.IO
import           System.Random
import           Control.Monad

randR :: Random a => (a, a) -> RandState a
randR (low, high) = do
      gen <- get
      let (x, gen') = randomR (low, high) gen
      put gen'
      return x

-- Succeed if randomly chosen point from square is inside circumscribed circle
piTrial :: RandState Bool
piTrial = do
  xcoor  <- randR (-1, 1)
  ycoor  <- randR (-1, 1)
  let determine :: Bool
      determine = (xcoor ^ 2) + (ycoor ^ 2) <= (1 :: Float)
  return determine

-- Perform n trials of the RandState function provided as the second argument,
--  and give back the number of successful trials
-- Hint: To perform the n trials, you can either use sequence from
--       Control.Monad, or you can use recursion
bernoulliTrials :: Int -> RandState Bool -> RandState Int
bernoulliTrials numbers piTrial = do
  length . filter (== True) <$> replicateM numbers piTrial

-- Approximate pi using n randomly chosen points
-- Hint: You will probably need to use the fromIntegral function to
--       convert Int into Double.
approxPi :: Int -> RandState Double
approxPi numbers = (*4) . (\x -> x / (fromIntegral numbers)) . fromIntegral <$> (bernoulliTrials numbers piTrial)
