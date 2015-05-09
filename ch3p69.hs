import Data.List hiding (intersperse)
import System.Random
import System.IO

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

sortByLength :: [[a]] -> [[a]]
sortByLength [] = []
sortByLength (xs:xss) = sortByLength shorter ++ [xs] ++ sortByLength longer
  where shorter = filter (\ys -> (length ys) < (length xs)) xss 
        longer = filter (\ys -> length ys >= length xs) xss

intersperse :: a -> [[a]] -> [a]
intersperse _ [] = []
intersperse _ (xs:[]) = xs
intersperse x (xs:xss) = xs ++ [x] ++ intersperse x xss

data Tree a = Node a (Tree a) (Tree a)
            | Empty
              deriving (Show)

height :: Tree a -> Int
height Empty = 0
height (Node _ l r) = 1 + max (height l) (height r)

data Direction = TurnL | TurnR | Straight deriving (Eq, Show)

data Point = Point { x :: Double, y :: Double } deriving (Eq, Show)

turns :: [Point]->[Direction]
turns ps@(a:b:c:_) = firstTurn ps : turns (tail ps)
turns _ = []

firstTurn :: [Point]->Direction
firstTurn ((Point ax ay):(Point bx by):(Point cx cy):_) =
  case signum discriminant of
   -1 -> TurnR
   0 -> Straight
   1 -> TurnL
  where discriminant = (ay - by) * (cx - bx) + (bx - ax) * (cy - by)

withoutTurns :: Direction -> [Point] -> [Point]
withoutTurns d ps@(a:b:c:rest)
  | firstTurn ps == d = withoutTurns d (a:c:rest)
  | otherwise = ps
withoutTurns _ ps = ps

addToHull :: Direction -> [Point] -> [Point] -> [Point]
addToHull d (p:ps) hs = addToHull d ps (withoutTurns d (p:hs))
addToHull _ [] hs = hs

findHull :: [Point] -> [Point]
findHull ps = (reverse.(drop 1)) upper ++ lower
  where xs = rmdups (sortBy xThenY ps)
        upper = addToHull TurnL xs []
        lower = addToHull TurnR xs []

xThenY :: Point -> Point -> Ordering
xThenY p1 p2
  | x p1 < x p2 = LT
  | x p1 > x p2 = GT
  | y p1 < y p2 = LT
  | y p1 > y p2 = GT
  | otherwise = EQ

-- remove duplicates from a sorted list
rmdups :: [Point] -> [Point]
rmdups (p1:p2:ps)
  | xThenY p1 p2 == EQ = rmdups (p1:ps)
  | otherwise = p1:rmdups (p2:ps)
rmdups ps = ps

createRandomPoints :: Int -> Int -> Int -> IO [Point]
createRandomPoints _ _ 0= return []
createRandomPoints maxX maxY n = do
  x <- createRandomPoint maxX maxY
  xs <- createRandomPoints maxX maxY (n - 1) 
  return (x:xs)

createRandomPoint :: Int -> Int -> IO Point
createRandomPoint maxX maxY = do
  x <- rand maxX
  y <- rand maxY
  return (Point (fromIntegral x) (fromIntegral y))

rand :: Int -> IO Int
rand maxBound = getStdRandom (randomR (0, maxBound))
  
main :: IO()
main = printhull 1000

calchull :: Int -> IO()
calchull n = do 
  ps <- createRandomPoints 40 20 n
  let hull = findHull ps in do
    return ()

printhull :: Int -> IO ()
printhull n = do
  cls
  ps <- createRandomPoints 40 20 n
  seqn (map (writeat ".") ps)
  goto (Point 0 21)
  hFlush stdout
  let hs = findHull ps in
   do
     seqn (map (writeat "*") hs)
     --goto (Point 0 23)
     --seqn (map (putStrLn.show) (sortBy xThenY ps))
     --putStrLn ""
     --seqn (map (putStrLn.show) hs)
     goto (Point 0 21)
     hFlush stdout
     r <- getChar
     if r == 'q' then (return ()) else main
     
goto :: Point -> IO()
goto (Point xx yy) =
  putStr ("\ESC[" ++ show (round yy) ++ ";" ++ show (round xx) ++ "H")

writeat :: String -> Point -> IO()
writeat xs p = do goto p
                  putStr xs

seqn :: [IO a] -> IO()
seqn [] = return ()
seqn (a:as) = do a
                 seqn as

cls :: IO ()
cls = putStr "\ESC[2J"


  



  
 
