{- | Project Euler Problem 3

Each new term in the Fibonacci sequence is generated by adding 
the previous two terms. 
By starting with 1 and 2, the first 10 terms will be:
1, 2, 3, 5, 8, 13, 21, 34, 55, 89, ...
By considering the terms in the Fibonacci sequence whose values 
do not exceed four million, find the sum of the even-valued terms.

-}

module Main where

import Data.List (unfoldr)

main :: IO ()
main = putStrLn $ "the Answer is " ++ show solveEuler2

solveEuler2 :: Integer
solveEuler2 =  sum . filter even . takeWhile inLimit $ fibs
    where inLimit    = (<= limit)

limit :: Integer
limit = 4000000

fibs :: [Integer]
fibs =  unfoldr nextFib (1, 2)
    where nextFib (a, b) = Just (a, (b, a+b))

