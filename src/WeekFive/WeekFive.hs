{- stack 
 --resolver lts-16.10
 --install-ghc
 exec ghci
-}

module WeekFive.WeekFive where
import WeekFive.ExprT
import WeekFive.Parser

-- Exercise 1
eval :: ExprT -> Integer
eval (Lit x) = x
eval (Add x y) = (eval x) + (eval y)
eval (Mul x y) = (eval x) * (eval y)

-- Exercise 2
evalStr :: String -> Maybe Integer
evalStr x = eval <$> (parseExp Lit Add Mul x)

-- Exercise 3
class Expr a where
  lit :: Integer -> a
  mul :: a -> a -> a
  add :: a -> a -> a

-- TODO
instance Expr ExprT where
  lit = 
  mul =
  add =
