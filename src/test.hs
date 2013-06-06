{-# OPTIONS -XRecordWildCards -XMultiParamTypeClasses -XFlexibleInstances #-}
import Control.Monad
import Control.Comonad
import Control.Comonad.Trans.Class
import Data.Functor.Identity
import Control.Concurrent
import Debug.Trace


class Indexable m i where
  (!) :: m a -> i -> a


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
  extract z = z ! (0::Int)
  extend f z = z { list = list' } where
    n = (length . list) z
    range = take n [0..]
    list' = map (f . flip shift z) range


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
  extend f = ListZipperT . extend' f' . runZipperT where
    f' = f . ListZipperT
    extend' = extend . shifted
    shifted f wz = ListZipper xs' i where
      ListZipper xs i = extract wz
      n = length xs
      range = take n [0..]
      xs' = map shifted_f range
      shifted_f i = f $ fmap (shift i) wz

instance ComonadTrans ListZipperT where
  lower = fmap extract . runZipperT


type ZZ a = ListZipperT ListZipper a

instance Show a => Show (ZZ a) where
  show = show . runZipperT

instance Indexable (ListZipperT ListZipper) (Int, Int) where
  z ! (x, y) = extract $ extract $ shift y $ runZipperT $ shiftT x z


fromList :: [[a]] -> ZZ a
fromList = ListZipperT . fmap (flip ListZipper 0) . flip ListZipper 0

toList :: ZZ a -> [[a]]
toList = list . fmap list . runZipperT


conway :: ZZ Char -> Char
conway z = case count of
             2 -> extract z
             3 -> '#'
             _ -> ' '
           where
  ListZipper zs y = runZipperT z
  ListZipper xs x = zs !! y
  indices :: [(Int, Int)]
  indices = [(x, y) | x <- [-1..1], y <- [-1..1]
                    , (x, y) /= (0, 0)]
  neighbours = map (z!) indices
  count = length $ filter (/= ' ') neighbours

life_step :: ZZ Char -> ZZ Char
life_step = extend conway


clear :: IO ()
clear = putStr "\x1B[2J\x1B[;H"

glider :: ZZ Char
glider = fromList [" #     ",
                   "  #    ",
                   "###    ",
                   "       ",
                   "       "]

glider_animation :: [ZZ Char]
glider_animation = helper glider where
  helper z = z:helper (life_step z)

main = forM_ glider_animation $ \screen -> do
         clear
         threadDelay 100000
         mapM_ putStrLn $ toList screen
