{-
   PLP - Laboratoire 1

   2.5 - Le loup, la chèvre et les choux

   @author Nicolas Crausaz
   @author Maxime Scharwath
-}
import System.Environment (getArgs)
import System.Exit(exitSuccess)
import Text.Printf (printf)

data Person = Wolf | Goat | Cabbage | Farmer deriving (Eq, Show)

newtype Boat = Boat {
  boatContent :: [Person]
} deriving (Show)

data Bank = Bank {
  bankContent :: [Person]
} deriving (Show)

data Game = Game {
  boat :: Boat,
  boatSide :: Bool, -- False = left, True = right
  leftBank :: Bank,
  rightBank :: Bank
} deriving (Show)

commands :: [([Char], [Char])]
commands =
  [ (":p", "afficher l'état du jeu"),
    (":l <passenger>", "charger la barque avec un passager"),
    (":u", "décharger la barque"),
    (":m", "déplacer la barque"),
    (":r", "réinitialiser le jeu"),
    (":q", "quitter le jeu"),
    (":h", "afficher l'aide")
  ]

displayHelp :: IO ()
displayHelp = do
  putStrLn "Liste des commandes :"
  mapM_ (uncurry (printf "%-20s %-10s\n")) commands

-- getCommand :: String -> [(String, IO (), String)]
-- getCommand arg = filter (\(x, _, _) -> x == arg) commands

-- TODO: Regles de couples autorisés

initGame :: Game
initGame = Game (Boat []) False (Bank [Wolf, Goat, Cabbage, Farmer]) (Bank [])

isPersonInBank :: Person -> Bank -> Bool
isPersonInBank p (Bank b) = p `elem` b

removePersonFromBank :: Person -> Bank -> Bank
removePersonFromBank p (Bank b) = Bank (filter (/= p) b)

loadPersonInBoat :: Game -> Person -> Game
loadPersonInBoat (Game (Boat b) side (Bank l) (Bank r)) p
    | not side && isPersonInBank p (Bank l) = Game (Boat (p:b)) side (removePersonFromBank p (Bank l)) (Bank r)
    | side && isPersonInBank p (Bank r) = Game (Boat (p:b)) side (Bank l) (removePersonFromBank p (Bank r))
    | otherwise = Game (Boat b) side (Bank l) (Bank r)


-- unloadPerson :: Game -> Person -> Game
-- unloadPerson game person = game { boat = boat { boatContent = filter (/= person) (boatContent boat) } }

-- display commands
-- helpCallback :: IO ()
-- helpCallback = do putStrLn "Commandes disponibles :" ; map (\(x, y) -> putStrLn $ x ++ " : " ++ y) commands


printGameState :: Game -> IO ()
printGameState game = do
  putStrLn "Etat du jeu :"
  putStrLn $ "  Barque : " ++ show (boatContent (boat game))
  putStrLn "  Rives : "
  putStrLn $ "    Gauche : " ++ show (bankContent (leftBank game))
  putStrLn $ "    Droite : " ++ show (bankContent (rightBank game))


gameLoop :: Game -> IO ()
gameLoop game = do
  putStrLn "Entrez une commande :"
  line <- getLine
  let args = words line
  case head args of
    ":p" -> printGameState game
    -- ":l" -> loadPersonInBoat game (args !! 1)
    ":u" -> printGameState game
    ":m" -> printGameState game
    ":r" -> printGameState game
    ":q" -> exitSuccess
    ":h" -> displayHelp
    _ -> putStrLn "Invalid command"

  gameLoop game


main = do
  putStrLn "Bienvenue dans le jeu du loup, la chèvre et les choux !"
  displayHelp
  printGameState initGame
  gameLoop initGame
