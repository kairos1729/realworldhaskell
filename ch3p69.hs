import Data.List hiding (intersperse)
import System.Random

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

firstTurn :: [Point]->Direction
firstTurn ((Point ax ay):(Point bx by):(Point cx cy):_) =
  case signum discriminant of
   -1 -> TurnR
   0 -> Straight
   1 -> TurnL
  where discriminant = (ay - by) * (cx - bx) + (bx - ax) * (cy - by)

turns :: [Point]->[Direction]
turns ps@(a:b:c:_) = firstTurn ps : turns (tail ps)
turns _ = []

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
  where xs = sortBy xThenY ps
        upper = addToHull TurnR xs []
        lower = addToHull TurnL xs []

xThenY :: Point -> Point -> Ordering
xThenY p1 p2
  | x p1 < x p2 = LT
  | x p1 > x p2 = GT
  | y p1 < y p2 = LT
  | y p1 > y p2 = GT
  | otherwise = EQ

createRandomPoints :: Int -> Int -> IO [Point]
createRandomPoints 0 _ = return []
createRandomPoints n maxXY = do
  x <- createRandomPoint maxXY
  xs <- createRandomPoints (n - 1) maxXY
  return (x:xs)

createRandomPoint :: Int -> IO Point
createRandomPoint maxXY = do
  x <- rand maxXY
  y <- rand maxXY
  return (Point (fromIntegral x) (fromIntegral y))

rand :: Int -> IO Int
rand maxBound = getStdRandom (randomR (0, maxBound))
  
main :: IO()
main = do
  ps <- createRandomPoints 100 20
  putStrLn (show ps)  
  putStrLn (show (findHull ps))

  

  



  
 
