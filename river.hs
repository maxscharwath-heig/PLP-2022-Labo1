{-
   PLP - Laboratoire 1

   2.5 - Le loup, la chèvre et les choux

   @author Nicolas Crausaz
   @author Maxime Scharwath
-}
import System.Environment (getArgs)
import System.Exit(exitSuccess)
import Text.Printf (printf)
import Data.Char (toLower)

data Person = Wolf | Goat | Cabbage | Farmer deriving (Eq, Show)

data Boat = Boat {
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

unloadBoat :: Game -> Game
unloadBoat (Game (Boat b) side (Bank l) (Bank r))
    | not side = Game (Boat []) side (Bank (b ++ l)) (Bank r)
    | otherwise = Game (Boat []) side (Bank l) (Bank (b ++ r))

moveBoat :: Game -> Game
moveBoat (Game (Boat b) side (Bank l) (Bank r))
  | validateMove (Game (Boat b) side (Bank l) (Bank r)) = Game (Boat b) (not side) (Bank l) (Bank r)
  | otherwise = Game (Boat b) side (Bank l) (Bank r)


printGameState :: Game -> IO ()
printGameState game = do
  putStrLn "Etat du jeu :"
  putStrLn $ "  Barque : " ++ show (boatContent (boat game)) ++ " sur la rive " ++ if boatSide game then "droite" else "gauche"
  putStrLn "  Rives : "
  putStrLn $ "    Gauche : " ++ show (bankContent (leftBank game))
  putStrLn $ "    Droite : " ++ show (bankContent (rightBank game)) ++ "\n\n"

validateMove :: Game -> Bool
validateMove game
  | not (boatSide game) = all (`elem` r) [Wolf, Goat] || all (`elem` r) [Cabbage, Goat]
  | boatSide game = all (`elem` l) [Wolf, Goat] || all (`elem` l) [Cabbage, Goat]
  | otherwise = True
  where l = bankContent (leftBank game)
        r = bankContent (rightBank game)


stringToPerson :: String -> Person
stringToPerson "loup" = Wolf
stringToPerson "chevre" = Goat
stringToPerson "choux" = Cabbage
stringToPerson "fermier" = Farmer
stringToPerson _ = error "Invalid person"

loadAction :: Game -> String -> Game
loadAction game name
  | [toLower s | s <- name] `elem` ["loup", "chevre", "choux", "fermier"] = loadPersonInBoat game (stringToPerson name)
  | otherwise = game

gameLoop :: Game -> IO ()
gameLoop game = do
  putStrLn "Entrez une commande :"
  line <- getLine
  let args = words line
  let sndArg = if length args > 1 then args !! 1 else ""
  case head args of
    ":p" -> printGameState game
    ":l" -> gameLoop $ loadAction game sndArg
    ":u" -> gameLoop $ unloadBoat game
    ":m" -> gameLoop $ moveBoat game
    ":r" -> gameLoop initGame
    ":q" -> exitSuccess
    ":h" -> displayHelp
    _ -> putStrLn "Invalid command"

  gameLoop game


main = do
  putStrLn "Bienvenue dans le jeu du loup, la chèvre et les choux !\n"
  displayHelp
  gameLoop initGame
