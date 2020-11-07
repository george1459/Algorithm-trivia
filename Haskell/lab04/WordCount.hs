import Data.List
import Data.Char

allToLower :: [Char] -> [Char]
allToLower cs = map toLower cs


-- breakdown a string at the point where it should be broken down
breakDown :: String -> (String, String)
breakDown cs = span (checkRight) (allToLower cs)

-- take the remainder of the broken-down string
takeLatter :: String -> String
takeLatter cs = drop 1 $ dropWhile (checkRight) (allToLower cs)

checkRight :: Char -> Bool
checkRight a = isLetter a || (== '\'') a || (== '-') a


-- sort input into different letters
sortString :: String -> [String]
sortString [] = [[]]
sortString ss = [fst $ breakDown ss] ++ (sortString $ takeLatter ss)

cleanSort :: String -> [String]
cleanSort ss = sort $ filter (/= "") $ sortString ss

groupDublicated :: [String] -> [[String]]
groupDublicated ss = groupBy (==) ss

getLength :: [[String]] -> [Int]
getLength ss = map length ss


-- Number ready for output
correspondingNum :: String -> [Int]
correspondingNum ss = getLength $ groupDublicated $ cleanSort ss

-- String ready for output
correspondingString :: String -> [String]
correspondingString ss = nub $ cleanSort ss


-- process the final output
output :: String -> String
output input = outputDuples $ zip (correspondingString $ input) (correspondingNum $ input)

-- process all the tuples
outputDuples :: [(String, Int)] -> String
outputDuples [] = []
outputDuples duples =
  case (head $ duples) of
    (word, number) -> word ++ " " ++ (show $ number) ++ "\n" ++ outputDuples (drop 1 duples)

main :: IO ()
main = do
  input <- getContents
  putStrLn $ output input
