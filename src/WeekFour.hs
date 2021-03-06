{- stack 
 --resolver lts-16.10
 --install-ghc
 exec ghci
-}

-- Exercise 1: Wholemeal Programming
fun1 :: [Integer] -> Integer
fun1 [] = 1
fun1 (x:xs)
  | even x = (x - 2) * fun1 xs
  | otherwise = fun1 xs

fun1' :: [Integer] -> Integer
fun1' = product . map (\x -> x - 2) . filter even

fun2 :: Integer -> Integer
fun2 1 = 0
fun2 n | even n = n + fun2 (n `div` 2)
       | otherwise = fun2 (3 * n + 1)

fun2' :: Integer -> Integer
fun2' = sum
      . filter even
      . takeWhile (/= 1)
      . iterate (\x -> if even x then x `div` 2 else 3 * x + 1)

-- Exercise 2: Folding with trees
data Tree a = Leaf | Node Integer (Tree a) a (Tree a)
  deriving (Show, Eq)

-- foldTree should generate a balanced binary tree from a list of values using foldr.
-- For example, one sample output might be the following:
--    foldTree "ABCDEFGHIJ" ==
--    Node 3
--       (Node 2
--         (Node 0 Leaf ’F’ Leaf)
--         ’I’
--         (Node 1 (Node 0 Leaf ’B’ Leaf) ’C’ Leaf))
--       ’J’
--       (Node 2
--         (Node 1 (Node 0 Leaf ’A’ Leaf) ’G’ Leaf)
--         ’H’
--         (Node 1 (Node 0 Leaf ’D’ Leaf) ’E’ Leaf))

balancedTreeHeight :: Int -> Integer
balancedTreeHeight n = floor (logBase 2 $ fromIntegral n::Double)

foldTree :: [a] -> Tree a
foldTree [] = Leaf
foldTree xs = Node height (foldTree half1) (head half2) (foldTree (tail half2))
  where
    listLen = length xs
    (half1, half2) = splitAt (listLen `div` 2) xs
    height = balancedTreeHeight listLen

-- Exercise 3: More Folds!
-- xor should return True if and only if there are an odd number of True
-- values contained in the input list.
xor :: [Bool] -> Bool
xor = foldr (\x y -> if x == True then (not y) else y) False

-- Implement map as a fold.
map' :: (a -> b) -> [a] -> [b]
map' f = foldr (\x y -> (f x):y) [] 

-- Exercise 4: Finding Primes
-- Read about the Sieve of Sundaram. Implement the algorithm using
-- function composition. Given an integer n, your function should
-- generate all the odd prime numbers up to 2n + 2.
cartProd :: [a] -> [b] -> [(a, b)]
cartProd xs ys = [(x,y) | x <- xs, y <- ys]

sieveSundaram :: Integer -> [Integer]
