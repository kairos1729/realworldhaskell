import System.Directory
import System.FilePath
import Control.Monad
import Control.Exception 
import System.IO
import Data.Time.Clock
import Control.Applicative
import Data.List
import GHC.Exts

main :: IO Integer
main = return 0

data Info = Info {
  infoPath :: FilePath
  , infoPerms :: Maybe Permissions
  , infoSize :: Maybe Integer
  , infoModTime :: Maybe UTCTime
  } deriving (Eq, Ord, Show)

traverse :: ([Info]->[Info])->FilePath->IO[Info]
traverse order path = do
  names <- getUsefulContents path
  contents <- mapM getInfo (path : map (path </>) names)
  liftM concat $ forM (order contents) $ \info -> do
    if isDirectory info && infoPath info /= path
      then traverse order (infoPath info)
      else return [info]

getUsefulContents :: FilePath -> IO [String]
getUsefulContents path = do
  names <- getDirectoryContents path
  return (filter (`notElem` [".", ".."]) names)

isDirectory :: Info -> Bool
isDirectory = maybe False searchable . infoPerms

maybeIO :: IO a -> IO (Maybe a)
maybeIO act = handleIo (\_ -> return Nothing) (Just `liftM` act)

handleIo :: (IOException -> IO a) -> IO a -> IO a
handleIo = handle 

getInfo :: FilePath -> IO Info
getInfo path = do
  perms <- maybeIO (getPermissions path)
  size <- maybeIO (bracket (openFile path ReadMode) hClose hFileSize)
  modified <- maybeIO (getModificationTime path)
  return (Info path perms size modified)
  
list order = do
  infos <- traverse order "."
  forM infos (\i -> putStrLn $ show (infoPath i) ++ " " ++ show (infoSize i))
  return ()
  -- where
    --order = (filter (maybe False (> 4000) . infoSize))
    --order = filter isDirectory
    --order = sortBy (\a b ->  transform a `compare` transform b)

type InfoP a = Info -> a

liftP2 :: (a -> b -> c) -> InfoP a -> InfoP b -> InfoP c
liftP2 o p1 p2 = o <$> p1 <*> p2

liftP :: (a -> b -> c) -> InfoP a -> b -> InfoP c
liftP o p x = liftP2 o p (constP x)  

constP :: a -> InfoP a
constP x  = \_ -> x

andP = liftP2 (&&)
orP = liftP2 (||)

equalP :: Eq a => InfoP a -> a -> InfoP Bool
equalP = liftP (==) 

greaterP :: Ord a => InfoP a -> a -> InfoP Bool
greaterP = liftP (>)

liftPath :: (FilePath -> a) -> InfoP a
liftPath p = p . infoPath

(==?) :: Eq a => InfoP a -> a -> InfoP Bool
(==?) = equalP

(&&?) = andP

(>?) :: Ord a => InfoP a -> a -> InfoP Bool
(>?) = greaterP

sizeP :: Info -> Integer
sizeP = maybe 0 id . infoSize

myTest = (liftPath takeExtension ==? ".hs") &&? (sizeP >? 400)
    
    
    
