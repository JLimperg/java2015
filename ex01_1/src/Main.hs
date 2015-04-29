module Main
( main
) where

import           Data.List (intercalate)

maxPrimesPerLine :: Int
maxPrimesPerLine = 10

maxNumber :: Int
maxNumber = 100

main :: IO ()
main = printPrimes

printPrimes :: IO ()
printPrimes =
    mapM_ putStrLn . format maxPrimesPerLine . takeWhile (<= maxNumber) $ primes

primes :: [Int]
primes = 2 : 3 : 5 : filter isPrime [6..]
  where
    isPrime n     = not . any (`divides` n) . takeWhile (<= isqrt n) $ primes
    x `divides` y = y `mod` x == 0

isqrt :: Int -> Int
isqrt = ceiling . sqrt . fromIntegral

format :: Int -> [Int] -> [String]
format itemsPerLine = map (intercalate " " .  map show) . chunk itemsPerLine

chunk :: Int -> [a] -> [[a]]
chunk n xs =
    case splitAt n xs of
      (prefix, [])     -> [prefix]
      (prefix, suffix) -> prefix : chunk n suffix
