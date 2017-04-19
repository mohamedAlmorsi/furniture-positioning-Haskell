add1  x y = x+ y
mult x y = x*y

data Fruit = Kiwi | Apple | Banana
isYellow :: Fruit-> Bool
isYello Kiwi = False
isYello Apple = False
isYellow Banana = True

type Point = (Int,Int)
getX (x,y)=x


data Nat = Zero | Succ Nat deriving Show 

natPlus Zero n = n
natPlus (Succ m) n = Succ (natPlus m n)

--data list a = Nil | Cons a (list a )
--data List a = Nil | Cons a (List a) deriving Show

--length1 Nil = 0
--length1 (Cons x y) = 1 + length1 y 


sumTree Nil = 0 
sumTree (BT this left right)= this + sumTree left + sumTree right
doubleTree Nil = Nil 
doubleTree (BT this left right) = BT (2*this) (doubleTree (left)) (doubleTree(right))

data Tree = Nil | BT Int Tree Tree deriving Show

helper x y | x>y = x 
		   | otherwise = y 

max1 Nil = 0
max1 (BT this left right) | this >= helper (max1(left)) (max1(right))  = this 
						  | otherwise = helper  (max1(right))  (max1(left))









 