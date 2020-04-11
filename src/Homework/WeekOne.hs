module Homework.WeekOne where

-- Exercise 1
--
-- converts positive integers to a list of digits. Note: results in the digits being reversed
toDigits :: Integer -> [Integer]
toDigits n
  | n <= 0 = []
  | otherwise = (n `mod` 10) : toDigits (n `div` 10)

-- also converts integers to a list of digits, but with the digits reversed
-- Note: I think this function isn't needed due to the implementation of toDigits which
-- ends up reversing the order of the digits by nature. Presumably the implementation the
-- course expected was one where we convert the number to a string first.
-- toDigitsRev :: Integer -> [Integer]
--
-- Exercise 2
--
-- doubles every other number in a list
doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther [] = []
doubleEveryOther [x] = [x]
doubleEveryOther (x:y:xs) = x : 2 * y : doubleEveryOther xs

-- Exercise 3
--
-- sums up all digits in a list of integers
sumDigits :: [Integer] -> Integer
sumDigits n = sum (n >>= toDigits)

-- Exercise 4
--
-- validates a credit card number
validate :: Integer -> Bool
validate = (\x -> x `mod` 10 == 0) . sumDigits . doubleEveryOther . toDigits

-- Exercise 5
--
-- Towers of Hanoi
type Peg = String

type Move = (Peg, Peg)

hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi n source dest storage
  | n <= 0 = []
  | n == 1 = [(source, dest)]
  | otherwise =
    hanoi (n - 1) source storage dest <> [(source, dest)] <>
    hanoi (n - 1) storage dest source

-- Exercise 6
--
-- Towers of Hanoi with 4 pegs
intSquareRoot :: Integer -> Integer
intSquareRoot n = aux n
  where
    aux x
      | x * x > n = aux (x - 1)
      | otherwise = x

getKValue :: Integer -> Integer
getKValue n = intSquareRoot (2 * n)

-- I stole the solution from https://cs.nyu.edu/courses/summer07/G22.2340-001/Presentations/McCann.pdf
-- Not sure how you prove that you need to use n - k with a k of sqrt(2n) to get the optimal
-- solution. I used a k value of 2 originally and it was a good solution but not optimal.
specialHanoi :: Integer -> Peg -> Peg -> Peg -> Peg -> [Move]
specialHanoi n source dest storage1 storage2
  | n <= 0 = []
  | n == 1 = [(source, dest)]
  | n == 2 = hanoi 2 source dest storage1
  | otherwise =
    specialHanoi (n - getKValue n) source storage1 dest storage2 <>
    hanoi (getKValue n) source dest storage2 <>
    specialHanoi (n - getKValue n) storage1 dest source storage2
