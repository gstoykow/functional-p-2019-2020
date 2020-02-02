module Stuff
  ( group
  , sortBy
  , groupBy
  , sortOn
  , groupOn
  , classifyOn
  , (&&&)
  , on
  ) where



insertBy :: (a -> a -> Ordering) -> a -> [a] -> [a]
insertBy _ x [] = [x]
insertBy o x (y:ys) 
  | (o x y) == LT || (o x y) == EQ = x:y:ys
  | otherwise =  y:(insertBy o x ys)

sortBy :: (a -> a -> Ordering) -> [a] -> [a]
sortBy _ [] = []
sortBy o (x:xs) = insertBy o x (sortBy o xs)

groupBy :: (a -> a -> Bool) -> [a] -> [[a]]
groupBy _ [] = []
groupBy p (x:xs) = (takeWhile (p x) (x:xs)):groupBy p (dropWhile (p x) xs)

group :: Eq a => [a] -> [[a]]
group xs = groupBy (==) xs

on :: (b -> b -> c) -> (a -> b) -> a -> a -> c
on fn f x y = fn (f x) (f y)

(&&&) :: (a -> b) -> (a -> c) -> a -> (b, c)
(&&&) fn f x  = (fn x, f x)

sortOn :: Ord b => (a -> b) -> [a] -> [a]
sortOn f = sortBy (compare `on` f)

groupOn :: Eq b => (a -> b) -> [a] -> [[a]]
groupOn f = groupBy ((==) `on` f)

classifyOn :: Ord b => (a -> b) -> [a] -> [[a]]
classifyOn _ [] = []
classifyOn f (x:xs) = (groupOn f (sortOn f (x:xs)))
