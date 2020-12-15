import           Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IntMap
import qualified Data.List

-- Thank god there are no spaces in the input
hackySplit :: Char -> String -> [String]
hackySplit s = words . map (\c -> if c == s then ' ' else c)

data IterState = S !(IntMap Int) !Int !Int
  deriving Show

partOne :: IO ()
partOne = do
  l:_ <- lines <$> getContents
  let start = map read $ hackySplit ',' l :: [Int]
      next (S occs n i) = case IntMap.lookup n occs of
        Nothing -> Just (0,    S (IntMap.insert n (i-1) occs) 0    (i+1))
        Just j  -> Just (next, S (IntMap.insert n (i-1) occs) next (i+1))
          where
            next = i-1-j
      startS    = S (IntMap.fromList $ zip (init start) [0..]) (last start) (length start)
      extension = Data.List.unfoldr next startS
      series    = start ++ extension
  print (series !! 2019)

partTwo :: IO ()
partTwo = do
  l:_ <- lines <$> getContents
  let start = map read $ hackySplit ',' l :: [Int]
      next (S occs n i) = case IntMap.lookup n occs of
        Nothing -> Just (0,    S (IntMap.insert n (i-1) occs) 0    (i+1))
        Just j  -> Just (next, S (IntMap.insert n (i-1) occs) next (i+1))
          where
            next = i-1-j
      startS    = S (IntMap.fromList $ zip (init start) [0..]) (last start) (length start)
      extension = Data.List.unfoldr next startS
      series    = start ++ extension
  print (series !! (30000000-1))

main = partTwo
