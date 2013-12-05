{- | Project Euler Problem 3

The prime factors of 13195 are 5, 7, 13 and 29.
What is the largest prime factor of the number 600851475143 ?

-}

module Main where

main :: IO ()
main = putStrLn $ "the Answer is " ++ show solveEuler3

solveEuler3 :: Integer
solveEuler3 = findLargestPrimFaktor number

number :: Integer
number = 600851475143

faktors :: Integer -> [Integer]
faktors n = filter dividesN [sroot, sroot-1 ..]
    where sr :: Double
          sr         = sqrt . fromIntegral $ n
          sroot      = ceiling sr
          dividesN d = n `mod` d == 0

findLargestFaktor :: Integer -> Integer
findLargestFaktor = head . faktors 

primFaktors :: Integer -> [Integer]
primFaktors = filter isPrim . faktors

isPrim :: Integer -> Bool
isPrim n = findLargestFaktor n == 1

findLargestPrimFaktor :: Integer -> Integer
findLargestPrimFaktor = head . primFaktors