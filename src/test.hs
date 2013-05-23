{-# OPTIONS -XRecordWildCards -XMultiParamTypeClasses #-}
import Control.Monad
import Control.Comonad

class Indexable m i where
  (!) :: m a -> i -> a


data ListZipper a = ListZipper { list :: [a]
                               , index :: Int
                               }

runList :: (ListZipper a -> ListZipper b) -> [a] -> [b]
runList f = list . f . flip ListZipper 0

shift :: Int -> ListZipper a -> ListZipper a
shift i (z@ListZipper{..}) = z { index = index' `mod` n } where
  n = length list
  index' = n + i

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

main = forM_ (take 20 $ iterate (step.) id) $ \steps -> do
         print $ runList steps "   #   "
