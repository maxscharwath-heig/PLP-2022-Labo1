{-
   PLP - Laboratoire 1

   2.2 - Décompte de mots

   @author Nicolas Crausaz
   @author Maxime Scharwath
-}
import System.IO (withFile, IOMode (ReadMode), hGetContents)
import System.Directory.Internal.Prelude (getArgs)
import Text.Printf (printf)

sumColons :: Num a => [[a]] -> [a]
sumColons [] = []
sumColons xs = foldl (zipWith (+)) (repeat 0) xs

getCounts :: String -> [Int]
getCounts contents = [length $ words contents, length $ lines contents, length contents]

readfile :: FilePath -> IO [Int]
readfile file = do
      contents <- readFile file
      return $ getCounts contents

displayHeaders headers = printf "%-10s %10s %10s %10s\n" (head headers) (headers !! 1) (headers !! 2) (headers !! 3)

displayFileRow row = printf "%-10s %10d %10d %10d\n" (fst row) (head $ snd row) (snd row !! 1) (snd row !! 2)

displayFooter footers = printf "%-10s %10d %10d %10d\n" "total" (head footers) (footers !! 1) (footers !! 2)

main :: IO ()
main =
   do
   args <- getArgs
   if null args then
      putStrLn "Please specify at least one file"
   else do
      files <- mapM readfile args

      -- Add the file name before the counts for each file
      let rows = zip args files

      -- Calculate the sum of all the counts
      let sums = sumColons files

      -- Display
      displayHeaders ["file", "word", "line", "byte"]
      mapM_ displayFileRow rows
      displayFooter sums
      