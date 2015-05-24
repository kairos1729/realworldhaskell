module Ch4p97 where
import Data.List
import Data.Char

asInt_fold :: String -> Int
asInt_fold ('-':cs) = - asInt_fold cs
asInt_fold cs = foldl' step 0 cs
  where step acc x
          | (not.isDigit) x = error (show x ++ " is not a digit")
          | acc > ((maxBound - digitVal) `div` 10) = error "overflow"
          | otherwise = (10 * acc) + digitVal
          where digitVal = digitToInt x
        
asInt_either :: String -> Either String Int
asInt_either ('-':cs) = case asInt_either cs of
                         Left e -> Left e
                         Right a -> Right (-a)
asInt_either cs = foldl' step (Right 0) cs
  where step (Left acc) _ = Left acc
        step (Right acc) x
          | (not.isDigit) x = Left (show x ++ " is not a digit")
          | acc > ((maxBound - digitVal) `div` 10) = Left "overflow"
          | otherwise = Right ((10 * acc) + digitVal)
          where digitVal = digitToInt x

concat_foldr :: [[a]] -> [a]
concat_foldr = foldr (++) []

groupBy_fold :: (a -> a -> Bool) -> [a] -> [[a]]
groupBy_fold p = foldl' step []
  where step [] next = [[next]]
        step xs next = if p (head lastList) next
                       then (init xs) ++ [(lastList ++ [next])]
                       else xs ++ [[next]]
          where lastList = last xs

-- foldl' is probably best since it can stop as soon as the predicate
-- is satisfied
any_foldl :: (a -> Bool) -> [a] -> Bool
any_foldl p = foldl' step False
  where step acc x = acc || p x

any_foldr :: (a -> Bool) -> [a] -> Bool
any_foldr p = foldr step False
  where step x acc = p x || acc

cycle_foldr :: [a] -> [a]
cycle_foldr xs = foldr (:) (cycle_foldr xs) xs

words_foldr :: [Char] -> [[Char]]
words_foldr xs =
  case foldr step [] xs of
   ([]:css) -> css
   css -> css
  where step (' ') [] = []
        step (' ') ([]:css) = []:css
        step (' ') css = []:css
        step c [] = [[c]]
        step c ([]:css) = [c]:css
        step c (cs:css) = (c:cs):css

unlines_foldr :: [[Char]] -> [Char]
unlines_foldr = foldr ((++).(++ "\n")) []

unlines_foldl :: [[Char]] -> [Char]
unlines_foldl = foldl' step []
  where step acc x = acc ++ x ++ "\n"

