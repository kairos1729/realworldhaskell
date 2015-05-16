import System.Environment (getArgs)
  
main :: IO ()
main = mainWith myFunction
       where mainWith function = do
               args <- getArgs
               case args of
                [input, output] -> interactWith function input output
                _ -> putStrLn "error: exactly two arguments needed"

             -- replace "id" with the name of our function below
             myFunction = transposeText

interactWith :: (String -> String) -> FilePath -> FilePath -> IO ()
interactWith function inputFile outputFile = do
  input <- readFile inputFile
  writeFile outputFile (function input)

safeHead :: [a] -> Maybe a
safeHead [] = Nothing
safeHead (x:_) = Just x

safeHead2 :: [a] -> Maybe a
safeHead2 xs = if not (null xs)
               then Just (head xs)
               else Nothing

safeHead3 :: [a] -> Maybe a
safeHead3 = safeVersion head

safeVersion :: ([a] -> a1) -> [a] -> Maybe a1
safeVersion listFunction xs = if not (null xs)
                              then Just (listFunction xs)
                              else Nothing

safeTail :: [a] -> Maybe [a]
safeTail [] = Nothing
safeTail (_:xs) = Just xs

safeTail2 :: [a] -> Maybe [a]
safeTail2 xs = if not (null xs)
               then Just (tail xs)
               else Nothing

safeLast :: [a] -> Maybe a
safeLast [] = Nothing
safeLast xs = Just (last xs)

safeLast2 :: [a] -> Maybe a
safeLast2 xs = if not (null xs)
               then Just (last xs)
               else Nothing

safeInit :: [a] -> Maybe [a]
safeInit = safeVersion init

splitWith :: (a -> Bool) -> [a] -> [[a]]
splitWith p xs =
  case first of
   [] -> []
   _ -> first : splitWith p rest
  where (_, ys) = break p xs
        (first, rest) = span p ys

firstWordsOfEachLine :: String -> String
firstWordsOfEachLine input = unlines (firstWords (lines input))

firstWords :: [String] -> [String]
firstWords [] = []
firstWords (cs:css) = firstWord cs : firstWords css

firstWord :: String -> String
firstWord [] = ""
firstWord cs = head (words cs)

transposeText :: String -> String
transposeText text = unlines (transpose maxlinelength ' ' css)
                   where css = lines text
                         maxlinelength = maxlength css

maxlength :: [[a]] -> Int
maxlength xs = maximum (lengths xs)
  where lengths [] = []
        lengths (y:ys) = length y : lengths ys

                           
transpose :: Int -> t -> [[t]] -> [[t]]
transpose width d xs = transpose_iter 0
  where transpose_iter i
          | i >= width = []
          | otherwise = valuesOrDefaultsAt i d xs : transpose_iter (i + 1) 

valuesOrDefaultsAt :: Int -> t -> [[t]] -> [t]
valuesOrDefaultsAt _ _ [] = []
valuesOrDefaultsAt n d (x:xs) = valueOrDefaultAt n d x : valuesOrDefaultsAt n d xs

valueOrDefaultAt :: Int -> a -> [a] -> a
valueOrDefaultAt n d xs = if (n >= (length xs))
                          then d
                          else xs !! n
