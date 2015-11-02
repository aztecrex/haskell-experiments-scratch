module ApplicativeStyle where

import Control.Applicative

f :: Int -> String
f = show

f2 :: Char -> Int -> String
f2 c x = c : '-' : show x


applEx1 = f <$> [1,2,3]

applEx2 = f2 <$> ['a','b','c'] <*> [1,2,3]

applEx3 = f2 <$> (['a','b','c'] *> ['y','z']) <*> [1,2,3]
-- ["y-1","y-2","y-3","z-1","z-2","z-3","y-1","y-2","y-3","z-1","z-2","z-3","y-1","y-2","y-3","z-1","z-2","z-3"]
applEx4 = f2 <$> (['a','b','c'] <* ['y','z']) <*> [1,2,3]
-- ["a-1","a-2","a-3","a-1","a-2","a-3","b-1","b-2","b-3","b-1","b-2","b-3","c-1","c-2","c-3","c-1","c-2","c-3"]

applEx5 = f2 <$> Just 'a' <*> Just 7
applEx6 = f2 <$> Nothing <*> Just 7
applEx7 = f2 <$> Just 'a' <*> Nothing
applEx8 = f2 <$> (Just 'a' <* Just 'b') <*> Just 7
applEx9 = f2 <$> (Just 'a' *> Just 'b') <*> Just 7
applEx10 = f2 <$> (Nothing *> Just 'b') <*> Just 7
-- Nothing
