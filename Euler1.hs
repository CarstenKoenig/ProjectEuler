{- | Project Euler Problem 1

If we list all the natural numbers below 10 that are multiples of 3 or 5, we get 3, 5, 6 and 9. The sum of these multiples is 23.

Find the sum of all the multiples of 3 or 5 below 1000.

-}

module Main where

main :: IO ()
main = putStrLn $ "the Answer is " ++ show solveEuler1

solveEuler1 :: Int
solveEuler1 = sum [ i | i <- [1..1000-1], i `mod` 3 == 0 || i `mod` 5 == 0 ]
