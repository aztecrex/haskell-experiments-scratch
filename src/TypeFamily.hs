{-#LANGUAGE MultiParamTypeClasses #-}

module TypeFamily where

data Fire = Charmander | Charmeleon | Charizard deriving Show
data Water = Squirtle | Wartortle | Blastoise deriving Show
data Grass = Bulbasaur | Ivysaur | Venusaur deriving Show

data FireMove = Ember | FlameThrower | FireBlast deriving Show
data WaterMove = Bubble | WaterGun deriving Show
data GrassMove = VineWhip deriving Show

class (Show nature, Show move) => Pokemon nature  move where
  pickMove :: nature -> move

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

battleWaterVsFire :: Water -> Fire -> IO ()
battleWaterVsFire water fire = do
  printBattle (show water) (show wmove) (show fire) (show fmove) (show water)
  where
    wmove = pickMove water :: WaterMove
    fmove = pickMove fire :: FireMove

battleFireVsWater = flip battleWaterVsFire

battleGrassVsWater :: Grass -> Water -> IO ()
battleGrassVsWater grass water = do
  printBattle (show grass) (show gmove) (show water) (show wmove) (show grass)
  where
    gmove = pickMove grass :: GrassMove
    wmove = pickMove water :: WaterMove

battleWaterVsGrass = flip battleGrassVsWater

battleFireVsGrass :: Fire -> Grass -> IO ()
battleFireVsGrass fire grass = do
  printBattle (show fire) (show fmove) (show grass) (show gmove) (show fire)
  where
    fmove = pickMove fire :: FireMove
    gmove = pickMove grass :: GrassMove

battleGrassVsFire = flip battleFireVsGrass

battleEx1 :: IO ()
battleEx1 = do
  battleWaterVsFire Squirtle Charmander
  battleFireVsWater Charmeleon Wartortle
  battleGrassVsWater Bulbasaur Blastoise
  battleWaterVsGrass Wartortle Ivysaur
  battleFireVsGrass Charmeleon Ivysaur
  battleGrassVsFire Venusaur Charizard
