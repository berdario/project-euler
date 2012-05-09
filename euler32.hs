import Data.List as List

digits :: Int -> [Int]
digits a
  | a' /= 0 = a `mod` 10 : digits a'
  | otherwise = [a]
  where
    a' = a `div` 10


pandigital :: [Int] -> Bool
--pandigital = ((==) 9) . length . (dropWhile ((== 0). head) . group . sort)
{-
pandigital x = all id bs && 9 == length bs
  where
    bs = map (\c -> (length c) == 1 && (head c) /= 0) $ List.group $ List.sort x
-}
pandigital x = l == (length nubbed) && l == 9 && (head nubbed)/=0
  where 
    nubbed = List.sort $ List.nub x
    l = length nubbed

main = do
  print $ pandigital $ digits 213456798