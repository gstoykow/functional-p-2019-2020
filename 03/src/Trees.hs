{-# LANGUAGE InstanceSigs #-}
{-# OPTIONS_GHC -fplugin=HLint #-} 

module Trees where

import Prelude
import Data.Monoid (Sum(..), All(..), Any(..), First(..))
import Data.Maybe (isJust)

data Tree a
  = Empty
  | Node a (Tree a) (Tree a)
  deriving Show

instance Eq a => Eq (Tree a) where

    (==) :: Eq a => Tree a -> Tree a -> Bool
    (==) Empty Empty = True
    (==) _ Empty = False
    (==) Empty _ = False
    (==) (Node v1 l1 r1) (Node v2 l2 r2) 
       | v1 /= v2 = False
       | otherwise = (==) l1 l2 && (==) r1 r2

insertOrdered :: Ord a => a -> Tree a -> Tree a
insertOrdered nv Empty = Node nv Empty Empty
insertOrdered nv (Node v l r) 
   | v >= nv = Node v (insertOrdered nv l) r
   | otherwise = Node v l (insertOrdered nv r)

listToBST :: Ord a => [a] -> Tree a
listToBST [] = Empty
listToBST lst = buildTree lst Empty True
   where buildTree [] t _ = t 
         buildTree (y:ys) t initFlag
            | initFlag = buildTree ys (insertOrdered y t) False
            | otherwise = buildTree ys (insertOrdered y t) False

isBST :: Ord a => Tree a -> Bool
isBST Empty = True
isBST (Node _ Empty Empty) = True
isBST (Node v Empty rn@(Node rv _ _)) 
   | v < rv = True && isBST rn
   | otherwise = False
isBST (Node v ln@(Node lv _ _) Empty) 
   | v >= lv = True && isBST ln
   | otherwise = False
isBST (Node v ln@(Node lv _ _) rn@(Node rv _ _))
   | v >= lv && v < rv = isBST ln && isBST rn
   | otherwise = False


findBST :: Ord a => a -> Tree a -> Bool
findBST _ Empty = False
findBST sv (Node v l r) 
   | v == sv = True
   | v > sv = findBST sv l
   | otherwise = findBST sv r 

mapTree :: (a -> b) -> Tree a -> Tree b
mapTree _ Empty = Empty
mapTree f (Node v l r) = Node (f v) (mapTree f l) (mapTree f r)

foldTree :: Monoid a => Tree a -> a
foldTree Empty = mempty
foldTree (Node v l r) = foldTree l <> v <> foldTree r

foldMapTree :: Monoid b => (a -> b) -> Tree a -> b
foldMapTree _ Empty = mempty
foldMapTree f (Node v l r) = foldMapTree f l <> f v <> foldMapTree f r

sumTree :: Num a => Tree a -> a
sumTree = getSum . foldMapTree Sum

allTree :: (a -> Bool) -> Tree a -> Bool
allTree p = getAll . foldMapTree (All . p)

treeToList :: Tree a -> [a]
treeToList = foldMapTree (:[])

elemTree :: Eq a => a -> Tree a -> Bool
elemTree = undefined

onMaybe :: (a -> Bool) -> a -> Maybe a
onMaybe p x = if p x then Just x else Nothing

findPred :: (a -> Bool) -> Tree a -> Maybe a
findPred = undefined

findAll :: (a -> Bool) -> Tree a -> [a]
findAll = undefined

ifJust :: Maybe a -> (a -> Maybe b) -> Maybe b
ifJust Nothing _ = Nothing
ifJust (Just x) f = f x

validateTree :: (a -> Maybe b) -> Tree a -> Maybe (Tree b)
validateTree = undefined

data Direction
  = L -- go left
  | R -- go right
  deriving (Show, Eq)

fetch :: [Direction] -> Tree a -> Maybe a
fetch = undefined

paths :: Tree a -> [(a, [Direction])]
paths = undefined
