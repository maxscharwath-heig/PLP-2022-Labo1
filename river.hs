import System.Environment (getArgs)
import System.Exit(exitSuccess)
{-
   PLP - Laboratoire 1

   2.5 - Le loup, la chèvre et les choux

   @author Nicolas Crausaz
   @author Maxime Scharwath
-}

commands =
  [ (":p", helpCallback, "afficher l'état du jeu"),
    (":l", helpCallback, "charger la barque avec un passager"),
    (":u", helpCallback, "décharger la barque"),
    (":m", helpCallback, "déplacer la barque"),
    (":r", helpCallback, "réinitialiser le jeu"),
    (":q", exitSuccess, "quitter le jeu"),
    (":h", helpCallback, "afficher l'aide")
  ]


getCommand :: String -> [(String, IO (), String)]
getCommand arg = filter (\(x, _, _) -> x == arg) commands

helpCallback = do putStrLn "Commandes disponibles :"
                  mapM_ (\(x, _, d) -> putStrLn $ "  " ++ x ++ " " ++ d) commands


gameLoop :: IO ()
gameLoop = do
  putStrLn "Entrez une commande :"
  line <- getLine
  let cmd = getCommand line
  if null cmd then putStrLn "Invalid command"
  else do
    let (_, callback, _) = head cmd
    callback

  gameLoop


main = do
  putStrLn "Bienvenue dans le jeu du loup, la chèvre et les choux !"
  helpCallback
  gameLoop
