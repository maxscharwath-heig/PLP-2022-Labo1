{-
   PLP - Laboratoire 1

   2.1 - Notation romaine

   @author Nicolas Crausaz
   @author Maxime Scharwath
-}
module Roman where

data RomanNumber = O | I | V | X | L | C | D | M 
   deriving(Show)

numberToRoman :: Integer -> RomanNumber
numberToRoman 0 = O
numberToRoman 1 = I
numberToRoman 5 = V
numberToRoman 10 = X
numberToRoman 50 = L
numberToRoman 100 = C
numberToRoman 500 = D
numberToRoman 1000 = M
-- numberToRoman n = 



