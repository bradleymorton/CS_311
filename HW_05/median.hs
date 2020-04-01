-- Bradley Morton
-- CS331 HW5 Excercise C
-- Spring 2020 for Dr. Chappell
-- If anything other than a number is input an exception will be thrown

import System.IO    
import Data.List 
import Data.Char 

main = do 
    putStrLn "Enter a list of integers, one on each line."
    putStrLn "I will compute the median of the list.\n"
    vect <- getVector
    if (null vect) then
        putStrLn "Empty list- no median"
    else do
        putStr "Median: "
        print (findMedian vect)
    putStrLn "\nCompute another median? [y/n]"
    input <- getLine
    if (map toLower input == "y") then
        main
    else do 
        putStrLn "Bye!"

-- findMedian
-- sorts list and returns median value
findMedian :: [Int] -> Int
findMedian [] = 0
findMedian vect = (sort vect) !! div (length vect) 2

-- getVector
-- gets input to list, stopping on a blank line
-- will throw exception if any other input given
getVector = do 
    putStr "Enter a number (blank line to end): "
    input <- getLine
    if null input
      then return []
    else do
      let num = read input
      next <- getVector
      return (num: next)