import Data.List
import Data.Char


allToLower :: [Char] -> [Char]
allToLower cs = map toLower cs


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
cleanSort ss = nub $ filter (/= "") $ sortString ss

cleanString :: [String] -> [String]
cleanString ss = map sort ss


-- analyze a list of strings, find out the ones we desire,
-- This will give us an output of the permutations we desire.

chooseCorrect :: [String] -> [String]
chooseCorrect [] = []
chooseCorrect (a: as) =
  case elem a as of
    True  -> a : chooseCorrect as
    False -> chooseCorrect as



-- given a string, a list of indexs, output the elements corresponding to the indexs
takeIndex :: [String] -> [Int] -> [String]
takeIndex ss is =
  case is of
    []       -> []
    (a : as) -> (take 1 (drop (head is) ss)) ++ (takeIndex ss (drop 1 is))


-- take a String and a list of the things we want to locate, give back a string within a string
giveApporpriate :: [String] -> [String] -> [[String]]
giveApporpriate ss []      = [[]]
giveApporpriate ss (i: is) = (takeIndex ss indexs) : giveApporpriate ss is
  where indexs = findIndices (== i) (cleanString ss)


core :: String -> [[String]]
core ss = filter (/= []) $ giveApporpriate (cleanSort ss) (nub $ chooseCorrect $ cleanString $ cleanSort ss)

-- from now on: take care of all the outputs

output0 :: [[String]] -> [[String]]
output0 xs = map sortAll xs where
  sortAll :: [String] -> [String]
  sortAll xs = sortOn id xs


output1 :: [[String]] -> [String]
output1 xs = map (intercalate ", " ) (output0 xs)

output2 :: [String] -> String
output2 xs = intercalate "\n" (sortOn head xs)

output :: String -> String
output xs = output2 $ output1 $ core xs


main :: IO ()
main = do
  input <- getContents
  putStrLn $ output input
