import qualified Data.MemoCombinators as Memo

factor :: Int -> Int -> Int
factor n 1 = 0
factor n x
  | x `mod` n == 0 = (1 +) $ factor n $ x `div` n
  | otherwise = 0


ffn :: Int -> Int -> Int
ffn = Memo.integral ffn'
      where
      ffn' n x
        | x < n = 0
        | x `mod` n == 0 = ffn n (x-1) + factor n x
        | otherwise = ffn n (x-1)
                      
ffn' n x
  | x < n = 0
  | x `mod` n == 0 = ffn' n (x-1) + factor n x
  | otherwise = ffn' n (x-1)                              

ff5 = ffn' 5
ff3 = ffn 3

result = length [1 | i <- [0..10000], ff5(2*i-1) < 2*ff5(i) ]

main = print result