module Conduit where

import Control.Monad
import Control.Monad.Identity
import Data.Conduit
import qualified Data.Conduit.List as CL


conduitEx1 :: Int
conduitEx1 = runIdentity $ CL.sourceList [1..10] $$ CL.fold (+) 0

source :: Source IO Int
source = CL.sourceList [1..4]

sink :: Sink String IO ()
sink = CL.mapM_ putStrLn

cmult2 :: Conduit Int IO Int
cmult2 = CL.map (*2)

cshow :: Conduit Int IO String
cshow = CL.map show

conduitEx2 = do
  source $= cmult2 $= cshow $$ sink

conduitEx3 = do
  source $$ cmult2 =$ cshow =$ sink
