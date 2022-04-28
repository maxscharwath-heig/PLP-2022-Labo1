{-
   PLP - Laboratoire 1

   2.1 - Notation romaine

   @author Nicolas Crausaz
   @author Maxime Scharwath

   REMARQUE: Ce programme autorise la saisie de numérotation romaine n'étant pas strictement valide, mais les résultats sont corrects.
   C'est-à-dire que les charactères ne faisant pas sens seront ignorés. Exemple: "IIMMII" deviendra "MMII" = 2002, qui est correct.
   Néamoins le cas d'invilidité où un caractère se répète à la suite plus de 3 fois est validé et lance une erreur (voir isRomanValid).
-}
module Roman
  (
    convertRomanToNumber,
    convertNumberToRoman,
    checkValidPredecessors, --remove,
    romToNum --remove
  )
where

import Data.List (group, isSubsequenceOf)
import Debug.Trace (trace)
import Distribution.Simple.Utils (xargs)

romToNum :: Char -> Integer
romToNum 'O' = 0
romToNum 'I' = 1
romToNum 'V' = 5
romToNum 'X' = 10
romToNum 'L' = 50
romToNum 'C' = 100
romToNum 'D' = 500
romToNum 'M' = 1000
romToNum xs = error "Invalid number"

{-
  Vérifie si le nombre romain n'est pas composé de plus de 3 caractères identiques
  à la suite (non autorisé, sauf pour M mais notre problème est limité des nombre <= 3999 donc ce cas n'apparait pas)

  Les symboles V, L, D ne peuvent pas être présent plus d'une fois.

  Il ne peut pas y avoir plus d'un symbole plus petit que le suivant (ex IIV -> non valide).
-}
-- 
isRomanValid :: String -> Bool
isRomanValid [] = True
isRomanValid xs
  | any (\x -> length x > 3) (group xs) = False
  | any (\x -> length (filter (== x) xs) > 1) ['V', 'L', 'D'] = False
  | not $ checkValidPredecessors xs = False
  | otherwise = True

checkValidPredecessors :: String -> Bool
checkValidPredecessors (x:y:z:xs)
  | length (x:y:z:xs) < 3 = True
  | romToNum x <= romToNum z && romToNum y < romToNum z = False     -- Erreur ici dans certains cas: Ex CCCLXXXIX = 389 mais donne error
  | otherwise = checkValidPredecessors (y:z:xs)
checkValidPredecessors _ = True


-- TODO: Seuls les symboles I,X,C peuvent précéder un symbole de poids plus grand

{-
  Converti une représentation en chiffre romain en un entier
-}
convertRomanToNumber :: String -> Integer
convertRomanToNumber [] = 0
convertRomanToNumber (x : xs)
  | not $ isRomanValid (x : xs) = error "Invalid number"
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

{-
  Converti un entier vers sa représentation en chiffre romain
-}
convertNumberToRoman :: Integer -> String
convertNumberToRoman n
   | n >= 4000 = "Number too big"
   | n <= 0 = ""
   | otherwise = snd (head dividors) ++ convertNumberToRoman (n - fst (head dividors))
      where dividors = filter (\x -> fst x <= n) convertions