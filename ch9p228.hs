import System.Directory
import System.FilePath
import Control.Monad
import Control.Exception 
import System.IO
import Data.Time.Clock
import Control.Applicative
import Data.List
import Data.Char
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

isNotDirectory :: Info -> Bool
isNotDirectory = not . isDirectory

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

postorder :: [Info] -> [Info]
postorder infos = case dirsFirst infos of
                   [] -> []
                   (i:is) -> is ++ [i]
  
dirsFirst :: [Info] -> [Info]
dirsFirst = partitionBy isDirectory

partitionBy :: (a -> Bool) -> [a] -> [a]
partitionBy p xs = filter p xs ++ filter (not.p) xs

type InfoP a = Info -> a

liftP2 :: (a -> b -> c) -> InfoP a -> InfoP b -> InfoP c
liftP2 o p1 p2 = o <$> p1 <*> p2

liftP :: (a -> b -> c) -> InfoP a -> b -> InfoP c
liftP o p x = liftP2 o p (constP x)  

constP :: a -> InfoP a
constP x  = \_ -> x

andP :: InfoP Bool -> InfoP Bool -> InfoP Bool
andP = liftP2 (&&)

orP :: InfoP Bool -> InfoP Bool -> InfoP Bool
orP = liftP2 (||)

equalP :: Eq a => InfoP a -> a -> InfoP Bool
equalP = liftP (==) 

greaterP :: Ord a => InfoP a -> a -> InfoP Bool
greaterP = liftP (>)

liftPath :: (FilePath -> a) -> InfoP a
liftPath p = p . infoPath

(==?) :: Eq a => InfoP a -> a -> InfoP Bool
(==?) = equalP

(&&?) :: InfoP Bool -> InfoP Bool -> InfoP Bool
(&&?) = andP

(||?) :: InfoP Bool -> InfoP Bool -> InfoP Bool
(||?) = orP

(>?) :: Ord a => InfoP a -> a -> InfoP Bool
(>?) = greaterP

sizeP :: Info -> Integer
sizeP = maybe 0 id . infoSize

myTestP :: InfoP Bool
myTestP = (liftPath takeExtension ==? ".hs") &&? (sizeP >? 400)
    
traverse2 dirP fileP filePath = traverse order2 filePath
  where order2 = filter dirAndFileP
        dirAndFileP = (isDirectory &&? dirP) ||? (isNotDirectory &&? fileP)

list2 dirP fileP  = do
  infos <- traverse2 dirP fileP "."
  forM (filter isNotDirectory infos)
    $ \i ->
       putStrLn $ show (infoPath i) ++ " " ++ show (infoSize i)
  return ()

data Iterate seed = Done {unwrap :: seed }
                   | Skip {unwrap :: seed }
                   | Continue {unwrap :: seed }
                   deriving (Show)

type Iterator seed = seed -> Info -> Iterate seed

foldTree :: Show a => Iterator a -> a -> FilePath -> IO a
foldTree iter initSeed path = do
  endSeed <- fold initSeed path
  return (unwrap endSeed)
  where
    fold seed subpath = getUsefulContents subpath >>= walk seed subpath

    walk seed parentpath (name:names) = do
      --putStrLn $ "walk: " ++ (show (name:names))
      let path' = parentpath </> name
      --putStrLn $ "path': " ++ (show path')
      info <- getInfo path'
      case iter seed info of
       done@(Done _) -> do
         --putStrLn "done"
         return done
       Skip seed' -> do
         --putStrLn "skip"
         walk seed' parentpath names
       Continue seed'
         | isDirectory info -> do
             --putStrLn $ "Dirfound: " ++ (show info) ++ " path'=" ++ (show path')
             next <- fold seed' path'
             --putStrLn $ "next=" ++ (show next)
             case next of
              done@(Done _) -> return done
              seed''        -> walk (unwrap seed'') parentpath names
         | otherwise -> do
             --putStrLn $ "Continue seed': " ++ (show seed')
             walk seed' parentpath names
    walk seed parentpath _ = return (Continue seed)

atMostThreHaskellRelatedFiles :: Iterator [FilePath]
atMostThreHaskellRelatedFiles paths info
  | length paths == 3 = Done paths
  | isDirectory info && takeFileName path == ".git" = Skip paths
  | extension `elem` [".hs", ".hi"] = Continue (path:paths)
  | otherwise = Continue paths
  where extension = map toLower (takeExtension path)
        path = infoPath info

countDirectories :: Iterator Integer
countDirectories count info =
  Continue (if isDirectory info
            then count + 1
            else count)

  
allDirs :: Iterator [FilePath]
allDirs paths info =
  Continue (if isDirectory info
            then infoPath info : paths
            else paths)
 
allstuff :: Iterator [FilePath]
allstuff paths info = Continue (infoPath info : paths)
