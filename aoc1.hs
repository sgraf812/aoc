import qualified Data.IntSet as S
getLines :: IO [String]
getLines = lines <$> getContents

main = do
  numbers <- map read . lines <$> getContents
  let complement = S.fromList $ map (2020 -) numbers
  putStr $ unlines [ show (p*q) | p <- numbers, p `S.member` complement, let q = 2020-p ]
