{-
   PLP - Laboratoire 1

   2.3 - Entiers de Peano

   @author Nicolas Crausaz
   @author Maxime Scharwath
-}
module Natural where

data Nat = Z | S Nat
   deriving (Show)

instance Num Nat where
   -- +
   S n + S m = n + m
   Z + S m = m
   S n + Z = n
   _ + _ = Z

   -- -
   S n - S m = n - m
   S n - Z = n
   _ - _ = Z

   -- *
   S n * S m = n * m
   _ * _ = Z

   -- ^
   -- (^) S n S m = n * m
   -- (^) _  _ = Z 

   fromInteger 0 = Z -- Pas vraiment juste (retourne S Z et on veut Z)
   fromInteger 1 = Z
   fromInteger n = S (fromInteger (n-1))

instance Eq Nat where
   -- ==
   Z == Z = True
   S n == S m = n == m
   _ == _ = False

   -- /=
   Z /= Z = True
   S n /= S m = n /= m
   _ /= _ = True

instance Ord Nat where
   -- <
   Z < S n = True
   S n < Z = False
   S n < S m = n < m
   _ < _ = False

   -- <=
   Z <= S n = True
   S n <= Z = False
   S n <= S m = n <= m
   _ <= _ = True

   -- >
   Z > S n = False
   S n > Z = True
   S n > S m = n > m
   _ > _ = False

   -- >=
   Z >= S n = False
   S n >= Z = True
   S n >= S m = n >= m
   _ >= _ = True

instance Enum Nat where 
   succ Z = S 1
   succ (S n) = S (n + 1)

   pred Z = Z
   pred (S n) = S (n - 1)



zero :: Nat 
zero = Z

isZero :: Nat -> Bool
isZero Z = True 
isZero _ = False