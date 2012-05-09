factors n = 1:factors' 2 n
factors' n x
  | (n >) $ floor $ sqrt $ fromIntegral x = [x]
  | divides && quotient /= n = n:quotient:factors' (n+1) x
  | divides = n:factors' (n+1) x
  | otherwise = factors' (n+1) x
  where
    divides = x `mod` n == 0
    quotient = x `div` n

smallest_triangle = smallest_triangle' 3 2
smallest_triangle' x ni n
  | (length fs) >= n = x
  | otherwise = smallest_triangle' (x+ni') ni' n
  where
    fs = factors x
    ni' = ni+1

main = do
  print (smallest_triangle 501)