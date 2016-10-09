{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Conway where

import Control.Comonad
import Control.Comonad.Trans.Class


class Indexable m i where
  (!) :: m a -> i -> a


-- a 1D world for cellular automata.
-- index is the current cell, all the others are neighbours.

data ListZipper a = ListZipper { list :: [a]
                               , index :: Int
                               } deriving Show

shift :: Int -> ListZipper a -> ListZipper a
shift i (z@ListZipper{..}) = z { index = index' `mod` n } where
  n = length list
  index' = index + i

instance Indexable ListZipper Int where
  ListZipper {..} ! i = list !! i' where
    i' = (index + i) `mod` length list

instance Functor ListZipper where
  fmap f (z@ListZipper{..}) = z { list = fmap f list }

instance Comonad ListZipper where
  -- inspect the current cell
  extract z = z ! (0::Int)
  
  -- run f on each cell, making each one the current cell.
  extend f z = z { list = list' } where
    n = (length . list) z
    range = take n [0..]
    list' = map (f . flip shift z) range


-- and now, a comonad-transformer version of all the above.

newtype ListZipperT w a = ListZipperT {
  runZipperT :: w (ListZipper a)
}

shiftT :: Functor w => Int -> ListZipperT w a -> ListZipperT w a
shiftT i = ListZipperT . fmap (shift i) . runZipperT

instance Comonad w => Indexable (ListZipperT w) Int where
  z ! i = xs !! i' where
    ListZipper xs index = (extract . runZipperT) z
    i' = (index + i) `mod` length xs

instance Functor w => Functor (ListZipperT w) where
  fmap f = ListZipperT . (fmap . fmap) f . runZipperT

instance Comonad w => Comonad (ListZipperT w) where
  extract = extract . extract . runZipperT
  
  extend :: forall a b. (ListZipperT w a -> b) -> ListZipperT w a -> ListZipperT w b
  extend f = ListZipperT . extend go . runZipperT where
    f' :: w (ListZipper a) -> b
    f' = f . ListZipperT
    
    go :: w (ListZipper a) -> ListZipper b
    go wz = ListZipper ys i where
      ListZipper xs i = extract wz
      
      n = length xs
      range = take n [0..]
      
      shifted_wzs :: [w (ListZipper a)]
      shifted_wzs = map (\j -> fmap (shift j) wz) range
      
      ys :: [b]
      ys = map f' shifted_wzs

instance ComonadTrans ListZipperT where
  lower = fmap extract . runZipperT


-- a 2D world for cellular automata.

type ZZ a = ListZipperT ListZipper a

instance Show a => Show (ZZ a) where
  show = show . runZipperT

instance Indexable (ListZipperT ListZipper) (Int, Int) where
  z ! (x, y) = extract $ extract $ shift y $ runZipperT $ shiftT x z


fromList :: [[a]] -> ZZ a
fromList = ListZipperT . fmap (flip ListZipper 0) . flip ListZipper 0

toList :: ZZ a -> [[a]]
toList = list . fmap list . runZipperT


-- demonstrate how lower and extract could have been used to index
-- along the two dimentions of the grid separately, instead of using
-- the specialized 2D indexing above.

index_horizontally :: Comonad w => Int -> ListZipperT w a -> a
index_horizontally i = (! i) . extract . runZipperT

index_vertically :: ComonadTrans t => Int -> t ListZipper a -> a
index_vertically i = (! i) . lower


-- the entire logic of Conway's Game of Life, in one function.

conway :: ZZ Char -> Char
conway z = case count of
             2 -> extract z
             3 -> '#'
             _ -> ' '
           where
  indices :: [(Int, Int)]
  indices = [(x, y) | x <- [-1..1], y <- [-1..1]
                    , (x, y) /= (0, 0)]
  neighbours = map (z!) indices
  count = length $ filter (/= ' ') neighbours

life_step :: ZZ Char -> ZZ Char
life_step = extend conway

life_animation :: ZZ Char -> [ZZ Char]
life_animation = iterate life_step
