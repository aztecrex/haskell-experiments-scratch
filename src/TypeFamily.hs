{-#LANGUAGE MultiParamTypeClasses #-}
{-#LANGUAGE TypeFamilies, FlexibleContexts #-}

module TypeFamily where

import Data.Tuple



class (Show a, Show (Move a)) => Pokemon a where
  data Move a :: *
  pickMove :: a -> Move a

data Fire = Charmander | Charmeleon | Charizard deriving Show
instance Pokemon Fire where
  data Move Fire = Ember | FlameThrower | FireBlast deriving Show
  pickMove Charmander = Ember
  pickMove Charmeleon = FlameThrower
  pickMove Charizard= FireBlast

data Water = Squirtle | Wartortle | Blastoise deriving Show
instance Pokemon Water where
  data Move Water = Bubble | WaterGun deriving Show
  pickMove Squirtle = Bubble
  pickMove _ = WaterGun

data Grass = Bulbasaur | Ivysaur | Venusaur deriving Show
instance Pokemon Grass where
  data Move Grass = VineWhip deriving Show
  pickMove _ = VineWhip

pickMoveEx :: IO ()
pickMoveEx = do
  print $ pickMove Squirtle
  print $ pickMove Charmander
  print $ pickMove Ivysaur

class (Pokemon winner, Pokemon loser) => Battle winner loser where
  battle :: winner -> loser -> IO ()
  battle w l = do
      printBattle (show w) (show wmove) (show l) (show lmove) (show w)
    where
      wmove = pickMove w
      lmove = pickMove l


printBattle :: String -> String -> String -> String -> String -> IO ()
printBattle pk1 mv1 pk2 mv2 winner = do
  putStrLn $ pk1 ++ " used " ++ mv1
  putStrLn $ pk2 ++ " used " ++ mv2
  putStrLn $ "Winner is: " ++ winner
  putStrLn ""


instance Battle Water Fire
instance Battle Fire Water where
  battle = flip battle

instance Battle Grass Water
instance Battle Water Grass where
  battle = flip battle

instance Battle Fire Grass
instance Battle Grass Fire where
  battle = flip battle


battleEx1 :: IO ()
battleEx1 = do
  battle Squirtle Charmander
  battle Charmeleon Wartortle
  battle Bulbasaur Blastoise
  battle Wartortle Ivysaur
  battle Charmeleon Ivysaur
  battle Venusaur Charizard
