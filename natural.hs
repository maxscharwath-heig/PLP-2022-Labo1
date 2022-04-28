{-
   PLP - Laboratoire 1

   2.3 - Entiers de Peano

   @author Nicolas Crausaz
   @author Maxime Scharwath
-}
module Natural where

data Nat = Z | S Nat
  deriving (Show)

-- Arithmetics
-- +
add :: Nat -> Nat -> Nat
add (S n) m = S (add n m)
add Z n = n

-- -
sub :: Nat -> Nat -> Nat
sub (S n) m = sub n m
sub Z n = n

-- *

multi :: Nat -> Nat -> Nat
multi (S n) m = add m (multi n m)
multi Z _ = Z
-- ^

pow :: Nat -> Nat -> Nat
pow n (S m) = multi n (pow n m)
pow _ Z = S Z

-- Comparisons
-- ==
eq :: Nat -> Nat -> Bool
eq (S n) (S m) = eq n m
eq Z Z = True
eq _ _ = False

-- /=
neq :: Nat -> Nat -> Bool
neq n m = not (eq n m)

-- <
lt :: Nat -> Nat -> Bool
lt (S n) (S m) = lt n m
lt Z (S n) = True
lt _ _ = False

-- <=
leq :: Nat -> Nat -> Bool
leq n m = lt n m || eq n m

-- >
gt :: Nat -> Nat -> Bool
gt n m = not (leq n m)

-- >=
geq :: Nat -> Nat -> Bool
geq n m = gt n m || eq n m

-- Convertions
-- Int -> Nat
intToNat :: Int -> Nat
intToNat n
  | n <= 0 = Z
  | otherwise = S (intToNat (n - 1))

-- Nat -> Int
natToInt :: Nat -> Int
natToInt Z = 0
natToInt (S n) = 1 + natToInt n

-- Nat -> String
natToString :: Nat -> String
natToString = show

-- Functions
-- succ
succ' :: Nat -> Nat
succ' (S n) = S (succ' n)
succ' Z = S Z

-- pred
pred' :: Nat -> Nat
pred' (S n) = S (pred' n)
pred' Z = Z

-- zero
zero :: Nat
zero = Z

-- isZero
isZero :: Nat -> Bool
isZero Z = True
isZero _ = False