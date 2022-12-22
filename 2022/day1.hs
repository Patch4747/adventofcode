-- day 1
-- advent of code
import System.Environment
import Data.List
import Data.List.Split


-- convert 2-D string list to 2-D int list
strToInt :: [[String]] -> [[Int]]
strToInt []     = []
strToInt (x:xs) = [map (read :: String -> Int) x] ++ strToInt xs


-- calculate total calories for each elf
sumCals :: [[Int]] -> [Int]
sumCals []     = []
sumCals (x:xs) = [sum x] ++ sumCals xs


main = do
  args <- getArgs
  contents <- readFile (head args)

  -- 2-D list
  let eachElfFood = splitOn [""] $ lines contents

  let sums = sumCals $ strToInt eachElfFood
  let top3 = take 3 $ reverse (sort sums)
  
  putStrLn $ "Elf with most calories has " ++ show (top3 !! 0) ++ "."
  putStrLn $ "Top three elves have " ++ show (sum top3) ++ " total calories."
