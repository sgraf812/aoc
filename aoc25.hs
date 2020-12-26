bruteForce :: Int -> Int
bruteForce pkey = go 1 0
  where
    go acc secret
      | acc == pkey = secret
      | otherwise   = go (acc*7 `mod` 20201227) (secret+1)

loop :: Int -> Int -> Int
loop subject secret = subject `seq` go 1 secret
  where
    go n 0 = n
    go n s = go (n*subject `mod` 20201227) (s-1)

partOne :: IO ()
partOne = do
  c:d:_ <- map read . lines <$> getContents
  let secretC = bruteForce c
  let secretD = bruteForce d
  print secretC
  print secretD
  print (loop c secretD)

main = partOne
