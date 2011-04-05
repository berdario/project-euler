#!/usr/bin/env runhaskell
import Control.Concurrent

reversible :: Int -> Bool
reversible a = reversible' da dar 0
	where
	da = digits a
	dar = reverse da
	
reversible' :: [Int] -> [Int] -> Int -> Bool
reversible' [] [] _ = True 
reversible' (d:da) (dr:dar) rem = (odd newd) && (reversible' da dar (newd `div` 10))
	where
	newd = d+dr+rem 

digits :: Int -> [Int]
digits a
	| a' /= 0 = a `mod` 10 : digits a'
	| otherwise = [a]
	where
		a' = a `div` 10
		
solve :: MVar Int -> MVar Int -> MVar Int
solve num count
	| n `mod` 10 /= 0 && reversible n = solve num (increase_count count)
	| n <= 1 = count
	| otherwise = solve num count
	where
	n = next_num 
		
next_num :: Mvar Int -> Io Int
next_num num = do
	n <- takeMVar num
	putMVar num (n-1)
	return n'

increase_count :: MVar Int -> MVar Int
increase_count count = do
	c <- takeMVar count
	putMVar count (c+1)
	
main :: IO()
main = print result
	where
	result = do
		count <- newMVar 0
		num <- newMVar 10000000
		forkIO (solve num count)
		count <- solve num count
		c <- takeMVar count
		return c 
