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
createFibonacciSeq x y = x : createFibonacciSeq y (x + y)

fibs2 :: [Integer]
fibs2 = createFibonacciSeq 0 1

-- Exercise 3
data Stream a = StreamCons a [a]

streamToList :: Stream a -> [a]
streamToList (StreamCons x xs) = x:xs

-- Custom instance of Show required to be able to print the Stream which is an infinite list.
-- In this case I've decided to implement it by showing the first 10 elements.
instance Show a => Show (Stream a) where
  show x = show $ take 10 $ streamToList x

-- Exercise 4
streamRepeat :: a -> Stream a
streamRepeat x = StreamCons x $ repeat x

streamMap :: (a -> b) -> Stream a -> Stream b
streamMap f (StreamCons x xs) = StreamCons (f x) (map f xs)

streamFromSeed :: (a -> a) -> a -> Stream a
streamFromSeed f seed = StreamCons seed infiniteList
  where infiniteList = streamToList $ streamFromSeed f (f seed)

-- Exercise 5
nats :: Stream Integer
nats = streamFromSeed (+1) 0

-- TODO
interleaveStreams :: Stream a -> Stream a -> Stream a

ruler :: Stream Integer
