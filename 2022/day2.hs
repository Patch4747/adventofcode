-- day 2
-- advent of code
import System.Environment
import Data.List.Split


-- points for the shape you play
shapeScore :: String -> Int
shapeScore "A" = 1  -- rock
shapeScore "B" = 2  -- paper
shapeScore "C" = 3  -- scissors
shapeScore _   = 0


-- define rules
beats :: String -> String -> Bool
beats "A" "C" = True  -- R > S
beats "B" "A" = True  -- P > R
beats "C" "B" = True  -- S > P
beats _ _     = False


-- based on opponent's shape, choose yours that satisfies second column
chooseShape :: [String] -> String
chooseShape [x,"X"] = head [y | y <- ["A","B","C"], x `beats` y]  -- you lose
chooseShape [x,"Y"] = x  -- draw
chooseShape [x,"Z"] = head [y | y <- ["A","B","C"], y `beats` x]  -- you win
chooseShape [x,_]   = ""


-- points for the outcome of the round
outcomeScore :: [String] -> Int
outcomeScore [] = 0
outcomeScore [_,y]
  | y == "Y"  = 3 -- draw
  | y == "Z"  = 6 -- you win
  | otherwise = 0 -- you lost


-- calculate total score
totalScore :: [[String]] -> Int
totalScore []     = 0
totalScore (x:xs) = shapeScore (chooseShape x) + outcomeScore x + totalScore xs


main = do
  args <- getArgs
  contents <- readFile $ head args

  -- 2-D list of moves
  let moves = map words $ lines contents

  putStrLn $ "Total score of moveset is " ++ show (totalScore moves) ++ "."
