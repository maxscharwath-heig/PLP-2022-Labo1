{-
   PLP - Laboratoire 1

   2.5 - Le loup, la chèvre et les choux

   @author Nicolas Crausaz
   @author Maxime Scharwath

   REMARQUE: Les actions non-autorisées n'affichent pas d'erreur, mais ne sont pas exécutées.
-}
import System.Environment (getArgs)
import System.Exit(exitSuccess)
import Text.Printf (printf)
import Data.Char (toLower)

{-
  Définition des structures et types
-}
data Person = Wolf | Goat | Cabbage | Farmer deriving (Eq)

instance Show Person where
  show Wolf = "loup"
  show Goat = "chevre"
  show Cabbage = "choux"
  show Farmer = "fermier"

data Boat = Boat {
  boatContent :: [Person]
} deriving (Show)

data Bank = Bank {
  bankContent :: [Person]
} deriving (Show)

data Game = Game {
  boat :: Boat,
  boatSide :: Bool, -- False = gauche, True = droite
  leftBank :: Bank,
  rightBank :: Bank
} deriving (Show)

{-
  Implémentation des commandes
-}
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
    | length b == 2 = Game (Boat b) side (Bank l) (Bank r)
    | not side && isPersonInBank p (Bank l) = Game (Boat (p:b)) side (removePersonFromBank p (Bank l)) (Bank r)
    | side && isPersonInBank p (Bank r) = Game (Boat (p:b)) side (Bank l) (removePersonFromBank p (Bank r))
    | otherwise = Game (Boat b) side (Bank l) (Bank r)

unloadBoat :: Game -> Game
unloadBoat (Game (Boat b) side (Bank l) (Bank r))
    | not side = Game (Boat []) side (Bank (b ++ l)) (Bank r)
    | otherwise = Game (Boat []) side (Bank l) (Bank (b ++ r))

moveBoat :: Game -> Game
moveBoat (Game (Boat b) side (Bank l) (Bank r))
  | Farmer `elem` b && validateMove (Game (Boat b)  (not side) (Bank l) (Bank r)) = Game (Boat b) (not side) (Bank l) (Bank r)
  | otherwise = Game (Boat b) side (Bank l) (Bank r)


printGameState :: Game -> IO ()
printGameState game = do
  putStrLn "Etat du jeu :"
  putStrLn $ "  Barque : " ++ show (boatContent (boat game)) ++ " sur la rive " ++ if boatSide game then "droite" else "gauche"
  putStrLn "  Rives : "
  putStrLn $ "    Gauche : " ++ show (bankContent (leftBank game))
  putStrLn $ "    Droite : " ++ show (bankContent (rightBank game)) ++ "\n\n"

{-
  Validation du déplacement

  Situations impossibles: si barque a loup et chevre 
-}
validateMove :: Game -> Bool
validateMove game
  | not (boatSide game) = not $ all (`elem` r) [Wolf, Goat] && all (`elem` r) [Cabbage, Goat]
  | boatSide game = not $ all (`elem` l) [Wolf, Goat] && all (`elem` l) [Cabbage, Goat]
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


{-
  Boucle du jeu
-}
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

main :: IO ()
main = do
  putStrLn "Bienvenue dans le jeu du loup, la chèvre et les choux !\n"
  displayHelp
  gameLoop initGame
