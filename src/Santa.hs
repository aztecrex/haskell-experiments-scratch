module Santa where

import Control.Concurrent.STM
import Control.Concurrent
import System.Random
import Control.Concurrent.MVar


meetInStudy :: Int -> IO ()
meetInStudy id = putStrLn ("Elf " ++ show id ++ " is meeting in the study.")

deliverToys :: Int -> IO ()
deliverToys id = putStrLn ("Reindeer " ++ show id ++ " is delivering toys.")


helper1 :: Group -> MVar () -> IO () -> IO ()
helper1 group mon do_task = do
  (in_gate, out_gate) <- joinGroup group
  passGate in_gate
  withMVar mon $ \_ -> do_task
  passGate out_gate

elf1, reindeer1 :: Group -> Int -> MVar () -> IO ()
elf1 gp id mon = helper1 gp mon (meetInStudy id)
reindeer1 gp id mon = helper1 gp mon (deliverToys id)

data Gate = MkGate Int (TVar Int)

newGate :: Int -> STM Gate
newGate n = do
  tv <- newTVar 0
  return $ MkGate n tv

passGate :: Gate -> IO ()
passGate (MkGate n tv) = do
  atomically $ do
    n_left <- readTVar tv
    check $ n_left > 0
    writeTVar tv $ n_left - 1

operateGate :: Gate -> IO ()
operateGate (MkGate n tv) = do
  atomically $ writeTVar tv n
  atomically $ do
    n_left <- readTVar tv
    check $ n_left == 0

data Group = MkGroup Int (TVar (Int, Gate, Gate))

newGroup :: Int -> IO Group
newGroup n = atomically $ do
  g1 <- newGate n
  g2 <- newGate n
  tv <- newTVar (n, g1, g2)
  return $ MkGroup n tv

joinGroup :: Group -> IO (Gate, Gate)
joinGroup (MkGroup n tv) = atomically $ do
  (n_left, g1, g2) <- readTVar tv
  check $ n_left > 0
  writeTVar tv (n_left - 1, g1, g2)
  return (g1, g2)

awaitGroup :: Group -> STM (Gate, Gate)
awaitGroup (MkGroup n tv) = do
  (n_left, g1, g2) <- readTVar tv
  check $ n_left == 0
  new_g1 <- newGate n
  new_g2 <- newGate n
  writeTVar tv (n, new_g1, new_g2)
  return (g1, g2)

forever :: IO () -> IO ()
forever act = do
  act
  forever act

randomDelay :: IO ()
randomDelay = do
  waitTime <- getStdRandom $ randomR (1, 1000000)
  threadDelay waitTime

elf :: Group -> Int -> MVar () -> IO ThreadId
elf gp id mon = forkIO $ forever $ do
  elf1 gp id mon
  randomDelay

reindeer :: Group -> Int -> MVar () -> IO ThreadId
reindeer gp id mon = forkIO $ forever $ do
  reindeer1 gp id mon
  randomDelay

santa :: Group -> Group -> MVar () -> IO ()
santa elf_gp deer_gp mon = do
    withMVar mon $ \_ -> putStrLn "------------------------------------"
    (task, (in_gate, out_gate)) <- atomically $ orElse
      (chooseGroup deer_gp "deliver toys")
      (chooseGroup elf_gp "meet in my study")
    withMVar mon $ \_ -> putStrLn ("Ho! Ho! Ho! Let's " ++ task ++ ".")
    operateGate in_gate
    operateGate out_gate
  where
    chooseGroup :: Group -> String -> STM (String, (Gate, Gate))
    chooseGroup gp task = do
      gates <- awaitGroup gp
      return (task, gates)

go :: IO ()
go = do
  pmon <- newMVar ()
  elf_group <- newGroup 3
  sequence_ [elf elf_group n pmon | n <- [1..10]]
  deer_group <- newGroup 9
  sequence_ [reindeer deer_group n pmon | n <- [1..9]]
  forever (santa elf_group deer_group pmon)
