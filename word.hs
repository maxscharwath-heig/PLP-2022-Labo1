{-
   PLP - Laboratoire 1

   2.2 - Décompte de mots

   @author Nicolas Crausaz
   @author Maxime Scharwath
-}
import System.IO (withFile, IOMode (ReadMode), hGetContents)
import System.Directory.Internal.Prelude (getArgs)
import Text.Printf (printf)

headers :: [String]
headers = ["file", "word", "line", "byte"]

sumColons :: Num a => [[a]] -> [a]
sumColons [] = []
sumColons xs = foldl (zipWith (+)) (repeat 0) xs

getCounts :: String -> [Int]
getCounts contents = [length $ words contents, length $ lines contents, length contents]

readfile :: FilePath -> IO [Int]
readfile file = do
      contents <- readFile file
      return $ getCounts contents

displayFileRow row = printf "%s     %d      %d      %d\n" (fst row) (head $ snd row) (snd row !! 1) (snd row !! 2)

main :: IO ()
main =
   do
   args <- getArgs
   if null args then
      putStrLn "Please specify at least one file"
   else do
      files <- mapM readfile args

      -- Add the file name before the counts for earch file
      let rows = zip args files

      print rows

      let sums = sumColons files

      mapM_ (printf "%s     ") headers
      putStrLn ""

      mapM_ displayFileRow rows

      printf "%s     %d     %d     %d\n" "total" (head sums) (sums !! 1) (sums !! 2)
      