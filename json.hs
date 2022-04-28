import System.IO
    ( IOMode(ReadMode), hClose, hGetContents, openFile )
import System.Environment ( getArgs )
import Data.Char (toUpper)
{-
   PLP - Laboratoire 1

   2.4 - Symboles JSON

   @author Nicolas Crausaz
   @author Maxime Scharwath
-}

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

instance Show JSONt where
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

isSpace :: Char -> Bool
isSpace c = c `elem` " \t\n\r"

isDigit c = c `elem` ['0'..'9']

-- isNull :: Char -> Bool

-- digit cs = token : tokenize rest
--   where
--     (token, rest) = span isDigit cs

-- [' ', '\t', '\n', '\r'

tokenize :: [Char] -> [JSONt]
tokenize [] = []
tokenize (x:xs) | isSpace x = tokenize xs
tokenize ('{':xs) = LBrace : tokenize xs
tokenize ('}':xs) = RBrace : tokenize xs
tokenize ('[':xs) = LBracket : tokenize xs
tokenize (']':xs) = RBracket : tokenize xs
tokenize (':':xs) = Colon : tokenize xs
tokenize (',':xs) = Comma : tokenize xs
tokenize ('n':'u':'l':'l':xs) = Null : tokenize xs
tokenize ('t':'r':'u':'e':xs) = Bool True : tokenize xs
tokenize ('f':'a':'l':'s':'e':xs) = Bool False : tokenize xs
tokenize ('"':xs) = String (takeWhile (/='"') xs) : tokenize (tail $ dropWhile (/='"') xs)
tokenize (c:xs) | isDigit c = Number (read (c:takeWhile isDigit xs)) : tokenize (dropWhile isDigit xs)
tokenize x = error ("Invalid JSON here -> " ++ show x)

main :: IO ()
main =
   do
      args <- getArgs
      if length args /= 1 then
         putStrLn "invalid file"
      else do
         handle <- openFile (head args) ReadMode
         contents <- hGetContents handle
         print (map show $ tokenize contents)
         hClose handle



