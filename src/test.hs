{-# OPTIONS -XRecordWildCards -XMultiParamTypeClasses -XFlexibleInstances #-}
import Control.Monad
import Control.Comonad
import Control.Comonad.Trans.Class
import Data.Functor.Identity
import System.Posix.Unistd
import Debug.Trace


class Indexable m i where
  (!) :: m a -> i -> a


data ListZipper a = ListZipper { list :: [a]
                               , index :: Int
                               } deriving Show

runList :: (ListZipper a -> ListZipper b) -> [a] -> [b]
runList f = list . f . flip ListZipper 0

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


conway1 :: ListZipper Char -> Char
conway1 z = case count of
              1 -> '#'
              _ -> ' '
            where
  indices :: [Int]
  indices = [-1,1]
  neighbours = map (z!) indices
  count = length $ filter (/= ' ') neighbours

step :: ListZipper Char -> ListZipper Char
step = extend conway1



newtype ListZipperT w a = ListZipperT {
  runZipperT :: w (ListZipper a)
}

runListT :: (ListZipperT Identity a -> ListZipperT Identity b) -> [a] -> [b]
runListT f = list
           . extract
           . runZipperT
           . f
           . ListZipperT
           . Identity
           . flip ListZipper 0

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


conwayT :: ListZipperT Identity Char -> Char
conwayT z = case count of
              1 -> '#'
              _ -> ' '
            where
  indices :: [Int]
  indices = [-1,1]
  neighbours = map (z!) indices
  count = length $ filter (/= ' ') neighbours

stepT :: ListZipperT Identity Char -> ListZipperT Identity Char
stepT = extend conwayT


type ZZ a = ListZipperT ListZipper a

instance Show a => Show (ZZ a) where
  show = show . runZipperT

instance Indexable (ListZipperT ListZipper) (Int, Int) where
  z ! (x, y) = extract $ extract $ shift y $ runZipperT $ shiftT x z

runList2 :: (ZZ a -> ZZ b) -> [[a]] -> [[b]]
runList2 f = list
           . fmap list
           . runZipperT
           . f
           . ListZipperT
           . fmap (flip ListZipper 0)
           . flip ListZipper 0

conway2 :: ZZ Char -> Char
conway2 z = case count of
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

step2 :: ZZ Char -> ZZ Char
step2 = extend conway2


clear :: IO ()
clear = putStr "\x1B[2J\x1B[;H"

main = forM_ (iterate (step2.) id) $ \steps -> do
         clear
         usleep 100000
         mapM_ putStrLn $ runList2 steps [" #     ",
                                          "  #    ",
                                          "###    ",
                                          "       ",
                                          "       "]
