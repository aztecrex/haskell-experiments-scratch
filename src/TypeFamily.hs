module TypeFamily where

data Fire = Charmander | Charmeleon | Charizard deriving Show
data Water = Squirtle | Wartortle | Blastoise deriving Show
data Grass = Bulbasaur | Ivysaur | Venusaur deriving Show

data FireMove = Ember | FlameThrower | FireBlast deriving Show
data WaterMove = Bubble | WaterGun deriving Show
data GrassMove = VineWhip deriving Show

pickFireMove :: Fire -> FireMove
pickFireMove Charmander = Ember
pickFireMove Charmeleon = FlameThrower
pickFireMove Charizard = FireBlast

pickWaterMove :: Water -> WaterMove
pickWaterMove Squirtle = Bubble
pickWaterMove _ = WaterGun

pickGrassMove :: Grass -> GrassMove
pickGrassMove _ = VineWhip

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
    wmove = pickWaterMove water
    fmove = pickFireMove fire

battleFireVsWater = flip battleWaterVsFire

battleGrassVsWater :: Grass -> Water -> IO ()
battleGrassVsWater grass water = do
  printBattle (show grass) (show gmove) (show water) (show wmove) (show grass)
  where
    gmove = pickGrassMove grass
    wmove = pickWaterMove water

battleWaterVsGrass = flip battleGrassVsWater

battleFireVsGrass :: Fire -> Grass -> IO ()
battleFireVsGrass fire grass = do
  printBattle (show fire) (show fmove) (show grass) (show gmove) (show fire)
  where
    fmove = pickFireMove fire
    gmove = pickGrassMove grass

battleGrassVsFire = flip battleFireVsGrass

battleEx1 :: IO ()
battleEx1 = do
  battleWaterVsFire Squirtle Charmander
  battleFireVsWater Charmeleon Wartortle
  battleGrassVsWater Bulbasaur Blastoise
  battleWaterVsGrass Wartortle Ivysaur
  battleFireVsGrass Charmeleon Ivysaur
  battleGrassVsFire Venusaur Charizard
