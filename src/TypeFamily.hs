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

printBattle :: String -> String -> String -> String -> String -> IO ()
printBattle pk1 mv1 pk2 mv2 winner = do
  putStrLn $ pk1 ++ " used " ++ mv1
  putStrLn $ pk2 ++ " used " ++ mv2
  putStrLn $ "Winner is: " ++ winner
  putStrLn ""

class (Show (Winner left right), Pokemon left, Pokemon right) =>
      Battle left right where
  type Winner left right :: *
  type Winner left right = left
  battle :: left -> right -> IO ()
  battle l r = do
      printBattle (show l) (show lmove) (show r) (show rmove) (show winner)
    where
      lmove = pickMove l
      rmove = pickMove r
      winner = pickWinner l r
  pickWinner :: left -> right -> (Winner left right)

instance Battle Water Fire where
  pickWinner left _ = left

instance Battle Fire Water where
  type Winner Fire Water = Water
  pickWinner = flip pickWinner

instance Battle Grass Water where
  pickWinner left _ = left

instance Battle Water Grass where
  type Winner Water Grass = Grass
  pickWinner = flip pickWinner

instance Battle Fire Grass where
  pickWinner left _ = left

instance Battle Grass Fire where
  type Winner Grass Fire = Fire
  pickWinner = flip pickWinner


battleEx1 :: IO ()
battleEx1 = do
  battle Squirtle Charmander
  battle Charmeleon Wartortle
  battle Bulbasaur Blastoise
  battle Wartortle Ivysaur
  battle Charmeleon Ivysaur
  battle Venusaur Charizard
