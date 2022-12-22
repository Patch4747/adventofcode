-- day 4
-- advent of code
import System.Environment
import Data.List.Split


-- convert string "a-b" to list [a,b]
toInts :: String -> [Int]
toInts x = map (read :: String -> Int) p
  where p = splitOn "-" x


-- check if one range is a subset of the other
isSubset :: [Int] -> [Int] -> Bool
isSubset r1 r2 = 
  let [f1,s1] = r1
      [f2,s2] = r2
  in  (f2 >= f1 && s2 <= s1) || (f1 >= f2 && s1 <= s2)


-- check if the two ranges overlap
overlaps :: [Int] -> [Int] -> Bool
overlaps r1 r2 =
  let [f1,s1] = r1
      [f2,s2] = r2
  in  (f2 >= f1 && f2 <= s1) || (f1 >= f2 && f1 <= s2)


-- determine if one string range is a subset of the other
findSubsetPairs :: [String] -> Bool
findSubsetPairs [s1, s2] = isSubset r1 r2
  where r1 = toInts s1
        r2 = toInts s2


-- determine if one string range overlaps with the other
findOverlapPairs :: [String] -> Bool
findOverlapPairs [s1, s2] = overlaps r1 r2
  where r1 = toInts s1
        r2 = toInts s2


-- count the number of Trues in a list
countTrues :: [Bool] -> Int
countTrues x = length (filter (==True) x)


main = do
  args <- getArgs
  contents <- readFile (head args)

  let pairs = map (splitOn ",") (lines contents)

  -- part 1
  let pairsWhoContain = map findSubsetPairs pairs

  putStrLn $ "Part1 -- number of encompassing ranges: " 
    ++ show (countTrues pairsWhoContain)

  -- part 2
  let pairsWhoOverlap = map findOverlapPairs pairs

  putStrLn $ "Part2 -- number of overlapping ranges: " 
    ++ show (countTrues pairsWhoOverlap)
