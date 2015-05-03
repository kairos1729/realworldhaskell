module Test where
  
fibs :: [Integer]
fibs = 1:1:[ a + b | (a,b) <- zip fibs (drop 1 fibs)]

fib :: Int -> Integer
fib n = fibs !! n

main :: IO()
main = do
  c <- getLine
  putStrLn (show (fib (read c :: Int)))
  main

lastButOne :: [a] -> a
lastButOne x = x !! ((length x) - 2) 
