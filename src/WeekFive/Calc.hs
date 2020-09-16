{- stack 
 --resolver lts-16.10
 --install-ghc
 exec ghci
-}

{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}

module WeekFive.Calc where
import WeekFive.ExprT
import WeekFive.Parser
import qualified WeekFive.StackVM as StackVM

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

instance Expr ExprT where
  lit x = Lit x
  mul x y = Mul x y
  add x y = Add x y

-- Exercise 4
instance Expr Integer where
  lit x = x
  mul = (*)
  add = (+)

instance Expr Bool where
  lit x
    | x <= 0    = False
    | otherwise = True
  mul = (&&)
  add = (||)

newtype MinMax = MinMax Integer deriving (Eq, Show)

instance Expr MinMax where
  lit x = MinMax x
  mul (MinMax x) (MinMax y) = MinMax $ min x y
  add (MinMax x) (MinMax y) = MinMax $ max x y

newtype Mod7 = Mod7 Integer deriving (Eq, Show)

instance Expr Mod7 where
  lit x = if (0 <= x && x <= 6) then Mod7 x else error "Provide a value from 0 to 6"
  mul (Mod7 x) (Mod7 y) = Mod7 $ (x * y) `mod` 7
  add (Mod7 x) (Mod7 y) = Mod7 $ (x + y) `mod` 7

testExp :: Expr a => Maybe a
testExp = parseExp lit add mul "(3 * -4) + 5"

testInteger :: Maybe Integer
testInteger = testExp

testBool :: Maybe Bool
testBool = testExp

testMM :: Maybe MinMax
testMM = testExp

testSat :: Maybe Mod7
testSat = testExp

-- Exercise 5
instance Expr StackVM.Program where
  lit x = [StackVM.PushI x]
  mul x y = x <> y <> [StackVM.Mul]
  add x y = x <> y <> [StackVM.Add]

compile :: String -> Maybe StackVM.Program
compile = parseExp lit add mul
