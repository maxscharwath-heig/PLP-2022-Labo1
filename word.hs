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

-- getNbWords :: String -> Int
-- getNbWords content = length $ words content
-- getNbLines :: String -> Int
-- getNbLines content = length $ lines content

-- getFileInfo :: FilePath -> IO [FilePath]
-- getFileInfo file = withFile file ReadMode (\handle -> do
--          contents <- hGetContents handle
--          -- printf "%s     %s     %s     %s\n" file (show $ getNbWords contents) (show $ getNbLines contents) (show $ length contents)
--          return [file, show $ getNbWords contents, show $ getNbLines contents, show $ length contents]
--       )

sumColons [] = []
sumColons xs = foldl (zipWith (+)) (repeat 0) xs

getCounts :: String -> [Int]
getCounts contents = [length $ words contents, length $ lines contents, length contents]

readfile :: FilePath -> IO [Int]
readfile file = do
      contents <- readFile file
      return $ getCounts contents


displayFileRow counts = printf "%s     %d      %d      %d\n" ("file" :: String) (head counts) (counts !! 1) (counts !! 2)


main =
   do
   args <- getArgs
   if null args then
      putStrLn "Please specify at least one file"
   else do
      files <- mapM readfile args
      let sums = sumColons files

      mapM_ (printf "%s     ") headers
      putStrLn ""
 
      mapM_ displayFileRow files

      printf "%s     %d      %d      %d\n" ("total" :: String) (head sums) (sums !! 1) (sums !! 2)
      