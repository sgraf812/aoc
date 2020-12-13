-- Thank god there are no spaces in the input
hackySplit :: Char -> String -> [String]
hackySplit s = words . map (\c -> if c == s then ' ' else c)

partOne :: IO ()
partOne = do
  l1:l2:_ <- lines <$> getContents
  let start = read l1 :: Int
      buses = map read $ filter (/= "x") $ hackySplit ',' $ l2 :: [Int]
      nexts = map (\b -> (b - start `mod` b, b)) buses
      next  = minimum nexts
  print (uncurry (*) next)

-- | For co-prime a, b this returns s,t such that a*s + b*t = gcd(a, b)
euclid :: Integer -> Integer -> (Integer, Integer)
euclid _a 0 = (1, 0) -- a = gcd
euclid a  b = (s, t)
  where
    (q, r)   = a `quotRem` b
    (s', t') = euclid b r
    s        = t'
    t        = s' - q * t'

addend :: Integer -> Integer -> Integer -> Integer
addend remainder m busi = remainder * (m `div` busi) * fst (euclid (m `div` busi) busi)

-- Chinese Remainder theorem. Find x such that
--   x `mod` bus1 =!= 0
--   x `mod` bus2 =!= 1
--   x `mod` bus3 =!= 2
--   ...
-- The following works because I figured that apparently the input is co-prime
partTwo :: IO ()
partTwo = do
  _l1:l2:_ <- lines <$> getContents
  let buses = hackySplit ',' l2
      m     = foldr lcm 1 $ map read $ filter (/="x") buses
      go _   "x" = 0
      go off b   = addend (m - off) m (read b)
      sol   = sum $ zipWith go [0..] buses
  print (sol `mod` m)

main = partTwo
