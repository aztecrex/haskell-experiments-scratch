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
