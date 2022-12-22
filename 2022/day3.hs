-- day 3
-- advent of code
import System.Environment
import Data.List
import Data.Maybe


-- make into 2-D list, where sublists of length 3
groupsOf3 :: [String] -> [[String]]
groupsOf3 [] = []
groupsOf3 x  = [take 3 x] ++ groupsOf3 (drop 3 x)


-- split string in half and find the one shared character
findShared :: String -> Char
findShared x = 
  let n = length x
      (f,s) = splitAt (n `div` 2) x
  in head [x | x <- f, y <- s, x == y]


-- find character that all strings in list share
findShared' :: [String] -> Char
findShared' (x:xs) = head $ foldr (\c acc -> acc `intersect` c) x xs


-- get integer value for character
getPriors :: Char -> Int
getPriors x
  | x `elem` letters = fromJust (x `elemIndex` letters) + 1
  | otherwise        = 0
  where letters = ['a'..'z'] ++ ['A'..'Z']


main = do
  args <- getArgs
  contents <- readFile (head args)

  -- part 1
  let sacks = lines contents

  let shared = map findShared sacks
  let priors = map getPriors shared

  putStrLn $ "Part1: Sum of priorities is " ++ show (sum priors) ++ "."

  -- part 2
  let groups = groupsOf3 sacks

  let priors2 = map getPriors $ map findShared' groups

  putStrLn $ "Part2: Sum of priorities is " ++ show (sum priors2) ++ "."
