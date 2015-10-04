module Parser where
import qualified Data.ByteString.Lazy.Char8 as L8
import qualified Data.ByteString.Lazy as L
import Data.Int
import Data.Word
import Data.Char
import Control.Applicative

data ParseState = ParseState {
  string :: L.ByteString
  , offset :: Int64
  } deriving (Show)

newtype Parse a = Parse {
  runParse :: ParseState -> Either String (a, ParseState)
  }

parse :: Parse a -> L.ByteString -> Either String a
parse p s = case runParse p (ParseState s 0) of
  Left err -> Left err
  Right (result, _) -> Right result 

identity :: a -> Parse a
identity x = Parse (\s -> Right (x, s))

bail :: String -> Parse a
bail err = Parse (\s -> Left ("byte offset " ++ show (offset s) ++ ": " ++ err))

getState :: Parse ParseState
getState = Parse (\s -> Right (s, s))

putState :: ParseState -> Parse ()
putState s = Parse (\_ -> Right((), s))

(==>) :: Parse a -> (a -> Parse b) -> Parse b
p ==> f =
  Parse (\s -> case runParse p s of
          Left err -> Left err
          Right (x, ns) -> runParse (f x) ns)
  
(==>&) :: Parse a -> Parse b -> Parse b
p1 ==>& p2 = p1 ==> \_ -> p2

instance Functor Parse where
  fmap f p = p ==> \r -> identity (f r)

parseByte :: Parse Word8
parseByte = getState ==> \s ->
  case L.uncons (string s) of
   Nothing -> bail "no more input"
   Just (b, r) ->
     putState (s {string = r, offset = (offset s) + 1}) ==> \_ ->
     identity b

w2c :: Word8 -> Char
w2c = chr . fromIntegral

parseChar :: Parse Char
parseChar = w2c <$> parseByte

peekByte :: Parse (Maybe Word8)
peekByte =  (fmap fst . L.uncons . string) <$> getState

peekChar :: Parse (Maybe Char)
peekChar = fmap w2c <$> peekByte

parseWhile :: (Word8 -> Bool) -> Parse [Word8]
parseWhile p = (fmap p <$> peekByte) ==> \mp ->
  if mp == Just True
  then parseByte ==> \b -> (b:) <$> parseWhile p
  else identity []

parseWhileWith :: (Word8 -> a) -> (a -> Bool) -> Parse [a]
parseWhileWith c p = fmap c <$> (parseWhile (p . c))

parseNat :: Parse Int
parseNat = parseWhileWith w2c isDigit ==> \digits ->
  if null digits
  then bail "No more input"
  else let n = read digits
       in if n < 0
          then bail "Integer overflow"
          else identity n
               
skipSpaces :: Parse ()
skipSpaces = parseWhileWith w2c isSpace ==>& identity ()

assert :: Bool -> String -> Parse ()
assert True _ = identity ()
assert False s = bail s

parseBytes :: Int -> Parse L.ByteString
parseBytes n = getState ==> \s ->
  let n1 = fromIntegral n
      (h, t) = L.splitAt n1 (string s)
      s1 = s {offset = offset s + L.length h, string = t}
  in putState s1 ==>&
     assert (L.length h == n1) "end of input" ==>&
     identity h

data Greymap = Greymap {
  greyWidth :: Int
  , greyHeight :: Int
  , greyMax :: Int
  , greyData :: L.ByteString
  } deriving (Eq)

instance Show Greymap where
  show (Greymap w h m _) = "Greymap " ++ show w ++ "x" ++ show h ++ " " ++ show m

parseRawPGM :: Parse Greymap
parseRawPGM = parseWhileWith w2c notWhite ==> \header ->
  skipSpaces ==>&
  assert (header == "P5") "invalid raw header" ==>&
  parseNat ==> \width ->
  skipSpaces ==>&
  parseNat ==> \height ->
  skipSpaces ==>&
  parseNat ==> \maxGrey ->
  skipSpaces ==>&
  parseBytes (width * height) ==> \bitmap ->
  identity (Greymap width height maxGrey bitmap)
  where notWhite = (`notElem` " \r\n\t")
                         
