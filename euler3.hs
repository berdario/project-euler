primes = map head $ iterate (\(x:xs) -> [y | y<-xs, y `mod` x /= 0 ]) [2..]

divides q n = n `mod` q == 0

divFully n q | q `divides` n = divFully (n `div` q) q
             | otherwise     = n

findMaxFactor' mf _ 1 = mf
findMaxFactor' mf (p:primes) n | p `divides` n = findMaxFactor' p primes $ divFully n p
                              | otherwise = findMaxFactor' mf primes n

findMaxFactor = findMaxFactor' 1 primes
