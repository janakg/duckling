{-# LANGUAGE BangPatterns #-}

foreign export javascript "fib" fib :: Int -> Int

main :: IO ()
main = do
  let x = fib 6 
  print x

fib :: Int -> Int
fib n = go 0 1 0
  where
    go !acc0 acc1 i
      | i == n = acc0
      | otherwise = go acc1 (acc0 + acc1) (i + 1)
