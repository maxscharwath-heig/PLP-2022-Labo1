{-
   PLP - Laboratoire 1

   2.2 - Décompte de mots

   @author Nicolas Crausaz
   @author Maxime Scharwath
-}
{-# LANGUAGE BlockArguments #-}
import System.IO (withFile, IOMode (ReadMode), hGetContents)
import System.Directory.Internal.Prelude (getArgs)
import System.Environment
import GHC.Base (IO(IO))
import System.Directory (getFileSize)
import System.FilePath (splitFileName)
import Text.Printf (printf)

-- get file size


headers :: [String]
headers = ["file", "word", "line", "byte"]

getNbWords content = length $ words content
getNbLines content = length $ lines content

getFileInfo file = withFile file ReadMode (\handle -> do  
        contents <- hGetContents handle  
        putStrLn (
           snd (splitFileName file) ++ " "
            ++ show(getNbWords contents) ++ " "
            ++ show(getNbLines contents) ++ " "
            )
      )

main =
   do
   args <- getArgs
   if null args then
      putStrLn "Please specify at least one file"
   else do
      -- Get file size

      --mapM_ (\x -> x ++ " ") headers   
      ---mapM_ getFileInfo args
      