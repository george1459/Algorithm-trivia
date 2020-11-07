numberof :: Int -> [Int] -> Int
numberof num = length . filter (== num)

main :: IO ()
main = do
  input <- getContents
  let numbers = map read (lines input) :: [Int]
  putStrLn $ (show $ numberof 2 numbers) ++ " " ++ (show $ numberof 3 numbers) ++ " " ++ (show $ numberof 4 numbers) ++ " " ++ (show $ numberof 5 numbers) ++ " " ++ (show $ numberof 6 numbers) ++ " " ++ (show $ numberof 7 numbers) ++ " " ++ (show $ numberof 8 numbers) ++ " " ++ (show $ numberof 9 numbers) ++ " " ++  (show $ numberof 10 numbers) ++ " " ++  (show $ numberof 11 numbers) ++ " " ++ (show $ numberof 12 numbers)
