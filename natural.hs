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

   fromInteger 0 = Z
   fromInteger n = S (fromInteger (n-1))



instance Eq Nat where
   -- ==
   Z == Z = True
   S n == S m = n == m
   _ == _ = False

   -- /=
   Z /= Z = True
   S n /= S m = not (S n == S m)
   _ /= _ = True