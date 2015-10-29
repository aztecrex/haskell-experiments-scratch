module Continuation where

import Control.Monad
import Control.Monad.Cont


add :: Int -> Int -> Cont k Int
add x y = return $ x + y

mult :: Int -> Int -> Cont k Int
mult x y = return $ x * y


contt :: ContT () IO ()
contt = do
  k <- do
    callCC $ \exit -> do
      lift $ putStrLn "Entry"
      exit $ \_ -> do
        putStrLn "Exit"
  lift $ putStrLn "Inside"
  lift $ k ()

callcc :: Cont String Integer
callcc = do
  a <- return 1
  b <- callCC (\k -> k 2)
  return $ a + b

ex1 :: IO ()
ex1 = print $ runCont (f >>= g) id
  where
    f = add 1 2
    g = mult 3

ex2 :: IO ()
ex2 = print $ runCont callcc show

ex3 :: IO ()
ex3 = runContT contt print

ex4 :: IO ()
ex4 = flip runContT return $ do
  lift $ putStrLn "alpha"
  callCC $ \k -> do
    lift $ putStrLn "insert"
    k ()
  lift $ putStrLn "beta"
  lift $ putStrLn "gamma"

ex5 :: IO ()
ex5 = flip runContT return $ do
  lift $ putStrLn "alpha"
  callCC $ \k -> do
    k ()
    lift $ putStrLn "insert"
  lift $ putStrLn "beta"
  lift $ putStrLn "gamma"

ex6 :: IO ()
ex6 = flip runContT return $ do
  lift $ putStrLn "alpha"
  num <- callCC $ \k -> do
    if 42 == 7 * 6
      then k 42
      else lift $ putStrLn "uh oh"
    return 43
  lift $ putStrLn "beta"
  lift $ putStrLn "gamma"
  lift $ print num

ex7 :: IO ()
ex7 = flip runContT return $ do
  lift $ putStrLn "alpha"
  (k, num) <- callCC $ \k -> let f x = k (f, x)
                             in return (f, 0)
  lift $ putStrLn "beta"
  lift $ putStrLn "gamma"
  if num < 5
    then (lift $ print num) >> k (num + 1) >> return ()
    else lift $ print num

aGoto :: IO ()
aGoto = flip runContT return $ do
  label <- callCC $ \k -> let f _ = k f
                          in return f
  lift $ putStrLn "hello"
  label ()
