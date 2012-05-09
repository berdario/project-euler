#!/usr/bin/env runhaskell
import Control.Parallel (par)

reversible :: Int -> Bool
reversible a = reversible' da dar 0
	where
	da = digits a
	dar = reverse da -- (reverse da) needs to get first the "innermost" element -> becames unnecessarily strict
	
reversible' :: [Int] -> [Int] -> Int -> Bool
reversible' [] [] _ = True 
reversible' (d:da) (dr:dar) rem = (odd newd) && (reversible' da dar (newd `div` 10))
	where
	newd = d+dr+rem 

digits :: Int -> [Int]
-- digits = map (\c -> read [c]) . show
digits a
	| a' /= 0 = a `mod` 10 : digits a'
	| otherwise = [a]
	where
		a' = a `div` 10
		
reverse_digits :: Int -> [Int]
reverse_digits a
	| numdigits > 0 = d:(reverse_digits' a (numdigits-1))
	| otherwise = [a]
	where 
	numdigits = floor (logBase 10 (fromIntegral a))
	d = a `div` (10 ^ numdigits)
	
reverse_digits' :: Int -> Int -> [Int]
reverse_digits' a numdigits
	| numdigits > 0 = (d `mod` 10):(reverse_digits' a (numdigits-1))
	| otherwise = [a `mod` 10]
	where
	d = a `div` (10 ^ numdigits)
	
		
solve :: Int -> Int
solve 1 = 0
solve n 
	| n `mod` 10 /= 0 && reversible n = (solve (n-1) +) $! 1
	| otherwise = solve (n-1)
	
maxval = 10000000

solve' :: Int -> Int
solve' 1 = (solve 2) `par` ((solve 3) `par` ((solve 2)+(solve 3)) )
solve' n
	| n >= maxval = 0
	| n `mod` 10 /= 0 && reversible n = (1 +) $! solve (n+2)
	| otherwise = solve (n+2)
	
main :: IO()
main = print (solve' 1)
