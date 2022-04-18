{-
   PLP - Laboratoire 1

   2.1 - Notation romaine

   @author Nicolas Crausaz
   @author Maxime Scharwath
-}
module Roman
  ( 
      convertRomanToNumber,
      convertNumberToRoman
  )
where

romToNum :: Char -> Integer
romToNum 'O' = 0
romToNum 'I' = 1
romToNum 'V' = 5
romToNum 'X' = 10
romToNum 'L' = 50
romToNum 'C' = 100
romToNum 'D' = 500
romToNum 'M' = 1000
romToNum xs = 0

convertRomanToNumber :: String -> Integer
convertRomanToNumber [] = 0
convertRomanToNumber (x : xs)
  | length (x : xs) == 1 = romToNum x
  | romToNum x < romToNum (head xs) = convertRomanToNumber xs - romToNum x
  | otherwise = convertRomanToNumber xs + romToNum x

convertions :: [(Integer, String)]
convertions =
  [ (1000, "M"),
    (900, "CM"),
    (500, "D"),
    (400, "CD"),
    (100, "C"),
    (90, "XC"),
    (50, "L"),
    (40, "XL"),
    (10, "X"),
    (9, "IX"),
    (5, "V"),
    (4, "IV"),
    (1, "I")
  ]

convertNumberToRoman :: Integer -> String
convertNumberToRoman n
   | n >= 4000 = "Number too big"
   | n <= 0 = ""
   | otherwise = snd (head dividors) ++ convertNumberToRoman (n - fst (head dividors))
      where dividors = filter (\x -> fst x <= n) convertions