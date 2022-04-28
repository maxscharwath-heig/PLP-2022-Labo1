{-
   PLP - Laboratoire 1

   2.4 - Symboles JSON

   @author Nicolas Crausaz
   @author Maxime Scharwath
-}
import Data.Char (toUpper)
import System.Environment (getArgs)
import System.IO (IOMode (ReadMode), hClose, hGetContents, openFile)

data JSONt
  = Null
  | Bool Bool
  | Number Integer
  | String String
  | LBrace
  | RBrace
  | LBracket
  | RBracket
  | Comma
  | Colon

instance Show JSONt where -- show value for JSONt type
  show Null = "null"
  show (Bool b) = if b then "true" else "false"
  show (Number n) = show n
  show (String s) = s
  show LBrace = "{"
  show RBrace = "}"
  show LBracket = "["
  show RBracket = "]"
  show Comma = ","
  show Colon = ":"

isSpace :: Char -> Bool -- isSpace determines if a char is an empty space
isSpace c = c `elem` " \t\n\r"

isDigit :: Char -> Bool -- isDigit determines if a character is a digit
isDigit c = c `elem` ['0' .. '9']

-- Transforms a string into a list of JSONt
tokenize :: [Char] -> [JSONt]
tokenize [] = []
tokenize (x : xs) | isSpace x = tokenize xs -- if the char is a space, skip it
tokenize ('{' : xs) = LBrace : tokenize xs
tokenize ('}' : xs) = RBrace : tokenize xs
tokenize ('[' : xs) = LBracket : tokenize xs
tokenize (']' : xs) = RBracket : tokenize xs
tokenize (':' : xs) = Colon : tokenize xs
tokenize (',' : xs) = Comma : tokenize xs
tokenize ('n' : 'u' : 'l' : 'l' : xs) = Null : tokenize xs
tokenize ('t' : 'r' : 'u' : 'e' : xs) = Bool True : tokenize xs
tokenize ('f' : 'a' : 'l' : 's' : 'e' : xs) = Bool False : tokenize xs
tokenize ('"' : xs) = String (takeWhile (/= '"') xs) : tokenize (tail $ dropWhile (/= '"') xs) -- take the string bewteen the quotes ""
tokenize (c : xs) | isDigit c = Number (read (c : takeWhile isDigit xs)) : tokenize (dropWhile isDigit xs) -- take all the digits and convert them to an integer
tokenize x = error ("Invalid JSON here -> " ++ show x)

main :: IO ()
main =
  do
    args <- getArgs
    if length args /= 1
      then putStrLn "invalid file"
      else do
         handle <- openFile (head args) ReadMode
         contents <- hGetContents handle
         --convert Token to String for pretty printing
         print (map show $ tokenize contents)
         hClose handle