import System.Environment (getArgs)
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
    (":q", helpCallback, "quitter le jeu"),
    (":h", helpCallback, "afficher l'aide")
  ]


getCommand :: String -> [(String, IO (), String)]
getCommand arg = filter (\(x, _, _) -> x == arg) commands

helpCallback = do putStrLn "Commandes disponibles :"
                  mapM_ (\(x, _, d) -> putStrLn $ "  " ++ x ++ " " ++ d) commands

main = do
   args <- getArgs
   if null args then error "Invalid command"
   else do 
      let command = getCommand (head args)
      if null command then error "Invalid command"
      else do
      let params = tail args

      -- get callback
      let (_, callback, _) = head command
      callback
