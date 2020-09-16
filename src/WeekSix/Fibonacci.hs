module WeekSix.Fibonacci where

-- Exercise 1
fib :: Integer -> Integer
fib 0 = 0 
fib 1 = 1
fib x = (fib (x - 1)) + (fib (x - 2))

fibs1 :: [Integer]
fibs1 = map fib [0..]

-- Exercise 2
-- In order to make the infinite list of Fibonacci numbers in a more performant way, we need
-- to avoid pattern matching so that we can make use of lazy evaluation.
--
-- This solution was taken from https://stackoverflow.com/a/21605696 as I'm not clever enough
-- to come up with this myself xD
--
-- createFibonacciSeq does not try to evaluate a concrete values and instead produces
-- a list which is the fibonacci sequence in a lazy manner. A particular value is only ever
-- evaluated if we try to access it from fibs2 by writing something like `fibs2 !! 20` (to 
-- get the 20th fibonacci number)
createFibonacciSeq :: Integer -> Integer -> [Integer]
createFibonacciSeq x y = x : fib' y (x + y)

fibs2 :: [Integer]
fibs2 = createFibonacciSeq 0 1

-- Exercise 3
