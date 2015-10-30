{-#LANGUAGE MultiParamTypeClasses #-}

module TypeFamily where

import Data.Tuple

data Fire = Charmander | Charmeleon | Charizard deriving Show
data Water = Squirtle | Wartortle | Blastoise deriving Show
data Grass = Bulbasaur | Ivysaur | Venusaur deriving Show

data FireMove = Ember | FlameThrower | FireBlast deriving Show
data WaterMove = Bubble | WaterGun deriving Show
data GrassMove = VineWhip deriving Show

class (Show creature, Show move) => Pokemon creature  move where
  pickMove :: creature -> move

instance Pokemon Fire FireMove where
  pickMove Charmander = Ember
  pickMove Charmeleon = FlameThrower
  pickMove Charizard = FireBlast

instance Pokemon Water WaterMove where
  pickMove Squirtle = Bubble
  pickMove _ = WaterGun

instance Pokemon Grass GrassMove where
  pickMove _ = VineWhip

printBattle :: String -> String -> String -> String -> String -> IO ()
printBattle pk1 mv1 pk2 mv2 winner = do
  putStrLn $ pk1 ++ " used " ++ mv1
  putStrLn $ pk2 ++ " used " ++ mv2
  putStrLn $ "Winner is: " ++ winner
  putStrLn ""

class (Pokemon creature1 move1, Pokemon creature2 move2) => Battle creature1 move1 creature2 move2 where
    battle :: creature1 -> creature2 -> IO (move1, move2)
    battle c1 c2 = do
        printBattle (show c1) (show m1) (show c2) (show m2) (show c1)
        return (m1,m2)
      where
        m1 = pickMove c1
        m2 = pickMove c2

instance Battle Water WaterMove Fire FireMove
instance Battle Fire FireMove Water WaterMove where
  battle a b = fmap swap $ flip battle a b

instance Battle Grass GrassMove Water WaterMove
instance Battle Water WaterMove Grass GrassMove where
  battle a b = fmap swap $ flip battle a b

instance Battle Fire FireMove Grass GrassMove
instance Battle Grass GrassMove Fire FireMove where
  battle a b = fmap swap $ flip battle a b


battleEx1 :: IO ()
battleEx1 = do
  battle Squirtle Charmander :: IO (WaterMove, FireMove)
  battle Charmeleon Wartortle :: IO (FireMove, WaterMove)
  battle Bulbasaur Blastoise :: IO (GrassMove, WaterMove)
  battle Wartortle Ivysaur :: IO (WaterMove, GrassMove)
  battle Charmeleon Ivysaur :: IO (FireMove, GrassMove)
  battle Venusaur Charizard :: IO (GrassMove, FireMove)
  putStrLn "Done Fighting"
