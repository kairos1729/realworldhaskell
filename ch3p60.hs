main :: IO()
main = return ()

data List a = Cons a (List a)
            | Nil
              deriving (Show)

fromList :: [a] -> List a
fromList (x:xs) = Cons x (fromList xs)
fromList [] = Nil

toList :: (List a) -> [a]
toList (Cons x xs) = x : toList xs
toList Nil = []

data Tree a = Node a (Tree a) (Tree a)
            | Empty
              deriving (Show)

height :: (Ord a, Num a) => Tree t -> a
height Empty = 0
height (Node _ l r) = 1 + max (height l) (height r)


data Tree2 a = Node2 a (Maybe (Tree2 a)) (Maybe (Tree2 a))
            deriving (Show)

height2 :: (Ord a, Num a) => Tree2 t -> a
height2 (Node2 _ Nothing Nothing) = 1
height2 (Node2 _ (Just l) Nothing) = 1 + height2 l
height2 (Node2 _ (Nothing) (Just r)) = 1 + height2 r
height2 (Node2 _ (Just l) (Just r)) = 1 + max (height2 l) (height2 r)



