-- PA5.hs  
-- By Bradley Morton, adapted from skeleton provided by Dr. Chappell
-- 2020-03-30
--
-- For CS F331 Spring 2020
-- Solutions to Assignment 5 Exercise B

module PA5 where
import Data.List

-- collatzRecurse
-- Takes integer, returns if equal to 1
-- Otherwise calls itself with different parameters
-- depending on if even or odd
collatzRecurse :: Integer -> Integer
collatzRecurse 1 = 0
collatzRecurse a
  | odd a     = collatzRecurse(3 * a + 1) + 1
  | otherwise = collatzRecurse(div a 2) + 1

-- collatzCounts
-- Counts number of steps to reach 1 with Collatz function
-- by calling recursive function above
collatzCounts :: [Integer]
collatzCounts = [collatzRecurse a | a <- [1..]]


-- findList
-- Takes two lists of same type
-- If first list is contiguous sublist of second list, return value
-- is the earliest index where a copy of the first exists
-- otherwise the return value is Nothing
findList :: Eq a => [a] -> [a] -> Maybe Int
findList a b = findIndex (isPrefixOf a) (tails b)
-- tails gives contiguous subsets starting from full and removing the 
-- first of each to go to last, as isPreFixOf checks if the first list
-- is a valid prefix at each point given by tails, with findIndex giving
-- the first value at which this occurs, or Nothing if it does not

-- compareLists
-- Given list of two lists will return number of places 
-- both lists have equivalent values in the same index
compareLists :: Eq a => [[a]] -> Integer
compareLists b
    | length b == 0 = 0
    | length (head b) <= 1 = 0
    | head (head b) == last (head b) = 1 + compareLists (tail b)
    | otherwise = compareLists (tail b)

-- ## operator
-- Transforms two lists into a list of lists
-- then calls compareLists to find number of equivalent items
-- in equal indices
a ## b = num where 
  num = compareLists (transpose [a, b]) --transpose turns [[1,2][3,4]] into [[1,3][2,4]]

-- filterAB
-- Takes two lists and a funtion 
-- Returns a list of elements from the second list 
-- where in the same index the function returns True for
-- the item in the first list
filterAB :: (a -> Bool) -> [a] -> [b] -> [b]
filterAB _ [] [] = []
filterAB _ _ []  = []
filterAB _ [] _  = []
filterAB func (a:aa) (b:bb)
   | func a       = b : filterAB func aa bb
   | otherwise = filterAB func aa bb


-- handleEvenOdd
-- Handles cases based on whether it is even or odd. 
handleEvenOdd [] = [0]
handleEvenOdd a = foldr(\b ls -> if (odd (fst b)) 
                                 then (snd b):ls 
                              else ls) [] (zip [1..] a)

-- sumEvenOdd
-- Takes list of numbers, returns tuple
-- First number is summation of even indices
-- Second number is summation of odd indices
sumEvenOdd :: Num a => [a] -> (a, a)
sumEvenOdd [] = (0,0)
sumEvenOdd c = (foldr1 (+) (a), foldr1 (+) (b))
  where a = handleEvenOdd c 
        b = handleEvenOdd (tail c)


