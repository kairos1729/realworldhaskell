module Prettify (
  Doc
  , empty
  , char
  , text
  , double
  , line
  , hcat
  , fsep
  , compact
  , pretty
  , docwidth
  , fill
  ) where

data Doc = Empty
         | Char Char
         | Text String
         | Line
         | Concat Doc Doc
         | Union Doc Doc
         deriving (Show, Eq)

empty :: Doc
empty = Empty

char :: Char -> Doc
char c = Char c

text :: String -> Doc
text "" = Empty
text s = Text s

double :: Double -> Doc
double d = text (show d)

line :: Doc
line = Line

(<>) :: Doc -> Doc -> Doc
Empty <> y = y
x <> Empty = x
x <> y = x `Concat` y

hcat :: [Doc] -> Doc
hcat = fold (<>)

fold :: (Doc -> Doc -> Doc) -> [Doc] -> Doc
fold f = foldr f empty

fsep :: [Doc] -> Doc
fsep = fold (</>)

(</>) :: Doc -> Doc -> Doc
x </> y = x <> softline <> y

softline :: Doc
softline = group line

group :: Doc -> Doc
group x = flatten x `Union` x

flatten :: Doc -> Doc
flatten (x `Concat` y) = flatten x `Concat` flatten y
flatten Line = Char ' '
flatten (x `Union` _) = flatten x
flatten other = other

compact :: Doc -> String
compact x = transform [x]
  where transform [] = ""
        transform (d:ds) =
          case d of
           Empty -> transform ds
           Char c -> c:transform ds
           Text s -> s ++ transform ds
           Line -> '\n' : transform ds
           a `Concat` b -> transform (a:b:ds)
           _ `Union` b -> transform (b:ds)
           
pretty :: Int -> Doc -> String
pretty width x = best 0 [x]
  where best col (d:ds) =         
          case d of
           Empty -> best col ds
           Char c -> c : best (col + 1) ds
           Text s -> s ++ best (col + length s) ds
           Line -> '\n' : best 0 ds
           a `Concat` b -> best col (a:b:ds)
           a `Union` b -> nicest col (best col (a:ds))
                          (best col (b:ds))

        best _ _ = ""

        nicest col a b | (width - least) `fits` a = a
                       | otherwise = b
          where least = min width col

fits :: Int -> String -> Bool
w `fits` _ | w < 0 = False
_ `fits` "" = True
_ `fits` ('\n':_) = True
w `fits` (_:cs) = (w - 1) `fits` cs

fill :: Int -> Doc -> Doc
fill w doc = padlines spaces doc
  where spaces = text (take (max 0 (w - (docwidth doc))) (repeat ' ' ))

padlines :: Doc -> Doc -> Doc
padlines p doc = p <> padlines_iter [doc]
  where padlines_iter [] = Empty
        padlines_iter (d:ds) =
          case d of
           Line -> d <> p <> padlines_iter ds
           a `Concat` b -> padlines_iter (a:b:ds)
           a `Union` _ -> padlines_iter (a:ds)
           x -> x <> padlines_iter ds

docwidth :: Doc -> Int
docwidth d = maximum (linewidths d)

linewidths :: Doc -> [Int]
linewidths doc = linewidths_iter 0 [doc]
  where linewidths_iter col (d:ds) =
          case d of
           Empty -> linewidths_iter col ds
           Char _ -> linewidths_iter (col + 1) ds
           Text s -> linewidths_iter (col + length s) ds
           Line -> col : linewidths_iter 0 ds
           a `Concat` b -> linewidths_iter col (a:b:ds)
           a `Union` _ -> linewidths_iter col (a:ds)
        linewidths_iter col _ = [col]

        
{- Give up here, i don't really understand what the exercises want -}


  

