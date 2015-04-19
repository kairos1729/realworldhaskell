main :: IO()
main = return ()

count :: Num a => [t] -> a
count (x:xs) = 1 + count xs
count [] = 0

mean :: Fractional a => [a] -> a
mean xs = mysum xs / fromIntegral (length xs)
          where mysum (y:ys) = y + mysum ys
                mysum [] = 0

palindrome :: [a] -> [a]
palindrome (x:xs) = x: palindrome xs ++ [x]
palindrome [] = []

ispalindrome :: Eq a => [a] -> Bool
ispalindrome [] = True
ispalindrome [x] = True
ispalindrome (x:xs) = (x == last xs) && ispalindrome (init xs)
