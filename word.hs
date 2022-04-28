{-
   PLP - Laboratoire 1

   2.2 - Décompte de mots

   @author Nicolas Crausaz
   @author Maxime Scharwath
-}
import System.IO (withFile, IOMode (ReadMode), hGetContents)
import System.Directory.Internal.Prelude (getArgs)
import Text.Printf (printf, PrintfType, PrintfArg)

{-
   Effectue une somme par "colonne" sur des listes
-}
sumColons :: Num a => [[a]] -> [a]
sumColons [] = []
sumColons xs = foldl (zipWith (+)) (repeat 0) xs

{-
   Compte le nombre de mots, de lignes et de bytes un string
-}
getCounts :: String -> [Int]
getCounts contents = [length $ words contents, length $ lines contents, length contents]

{-
   Lis un fichier et récupère ses informations (getCounts)
-}
readfile :: FilePath -> IO [Int]
readfile file = do
      contents <- readFile file
      return $ getCounts contents

{- 
   Fonctions d'affichage
-}
displayHeaders :: (Text.Printf.PrintfArg t1, Text.Printf.PrintfType t2) => [t1] -> t2
displayHeaders headers = printf "%-10s %10s %10s %10s\n" (head headers) (headers !! 1) (headers !! 2) (headers !! 3)

displayFileRow :: (PrintfArg a, PrintfArg t1, PrintfType t2) => (a, [t1]) -> t2
displayFileRow row = printf "%-10s %10d %10d %10d\n" (fst row) (head $ snd row) (snd row !! 1) (snd row !! 2)

displayFooter :: (PrintfArg t1, PrintfType t2) => [t1] -> t2
displayFooter footers = printf "%-10s %10d %10d %10d\n" "total" (head footers) (footers !! 1) (footers !! 2)

main :: IO ()
main =
   do
   args <- getArgs
   if null args then
      putStrLn "Please specify at least one file"
   else do
      files <- mapM readfile args

      -- Ajoute le nom de chaque fichier avant les valeurs comptées
      let rows = zip args files

      -- Calcule la somme de chaque colonne
      let sums = sumColons files

      -- Affichage du tableau
      displayHeaders ["file", "word", "line", "byte"]
      mapM_ displayFileRow rows
      displayFooter sums
      