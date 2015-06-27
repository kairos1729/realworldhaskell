module GlobRegex
       (
         globToRegex
       , matchesGlob
       , namesMatching
       ) where

import Data.Functor
import Control.Monad
import Control.Applicative
import Text.Regex.Posix ((=~))
import Data.Char
import System.Directory (doesDirectoryExist, doesFileExist,
                         getCurrentDirectory, getDirectoryContents)
import System.Posix.Files (fileExist)
import System.FilePath (
  dropTrailingPathSeparator,
  splitFileName,
  (</>),
  pathSeparator)

import Control.Exception (handle, IOException)
import Control.Monad (forM)

data CaseSensitivity = IsCaseSensitive | IsNotCaseSensitive

type GlobError = String

type G2RResult = Either GlobError String

globToRegex :: CaseSensitivity->String->G2RResult
globToRegex s cs = (\x -> '^' :  x ++ "$") <$> globToRegex' s cs
        
globToRegex' :: CaseSensitivity -> String -> Either GlobError String
globToRegex' _ "" = Right ""

globToRegex' s ('*':'*':cs) = (\x->".*" ++ x) <$> globToRegex' s cs
globToRegex' s ('*':cs) = (\x->".*" ++ x) <$> globToRegex' s cs

globToRegex' s ('?':cs) = (\x->"." ++ x) <$> globToRegex' s cs

globToRegex' s ('[':'!':c:cs) = (\x->"[^" ++ x)
                                <$> charClass s cs
                                >>= \y->uplowhead s (c : y)
globToRegex' s ('[':c:cs) = (\x->"[" ++ x)
                            <$> charClass s cs
                            >>= \y->uplowhead s (c : y)
globToRegex' _ ('[':_) = Left "unterminated character class"

globToRegex' s (c:cs) = (\x->escape s c ++ x) <$> globToRegex' s cs

escape :: CaseSensitivity -> Char -> String
escape s c | c `elem` regexChars = '\\' : [c]
           | otherwise = uplowheadCharClass s [c]
  where regexChars = "\\+()^$.{}]|"

charClass :: CaseSensitivity -> String -> G2RResult
charClass s (']':cs) = (\x->']' : x) <$> globToRegex' s cs
charClass s (c:cs) =  charClass s cs >>= \x->uplowhead s (c : x)
charClass _ [] = Left "unterminated character class"

uplowheadCharClass :: CaseSensitivity->[Char]->[Char]
uplowheadCharClass _ [] = []
uplowheadCharClass IsNotCaseSensitive (c:cs) = '[':(toLower c):(toUpper c):']':cs
uplowheadCharClass IsCaseSensitive cs = cs

uplowhead :: CaseSensitivity->[Char]->G2RResult
uplowhead _ [] = Right []
uplowhead IsNotCaseSensitive (c:cs) = Right $ (toLower c):(toUpper c):cs
uplowhead IsCaseSensitive cs = Right cs

matchesGlob :: String -> CaseSensitivity -> Either GlobError (FilePath -> Bool)
matchesGlob pat s = (globToRegex s pat) >>= \regex -> Right (\fp -> fp =~ regex) 

isPattern :: String -> Bool
isPattern = any (`elem` "[*?")

namesMatching :: String -> IO (Either GlobError [String])
namesMatching pat
  | not (isPattern pat) = do
      exists <- doesNameExist pat
      return (if exists then Right [pat] else Right [])
  | otherwise = findmatching
  where
    (dirName, baseName) = splitFileName pat

    findmatching = do
      if dirName == ""
        then
        do
          curDir <- getCurrentDirectory
          listMatches curDir baseName
        else
        do
          dirs <- if isPattern dirName
                  then namesMatching (dropTrailingPathSeparator dirName)
                  else return (Right [dirName])
          let listDir = if isPattern baseName
                        then listMatches
                        else listPlain
          case dirs of
           (Left x) -> return (Left x)
           (Right ds) -> do
             pathNames <- forM ds $ \dir -> do
               baseNames <- listDir dir baseName
               recursiveNames <- recursive dir
               return $ case (baseNames >>= \b->
                 recursiveNames >>= \r ->
                 Right((map (dir </>) b) ++ r)) of
                        (Left x) -> [x]
                        (Right x) -> x
             return (Right (concat pathNames))

    recursive dir =
      case baseName of
       ('*':'*':_) -> namesMatching $ dir </> "*" </> baseName
       _ -> return (Right [])
      

{--
doesNameExist :: FilePath -> IO Bool
doesNameExist name = do
  fileExists <- doesFileExist name
  if fileExists
    then return True
    else doesDirectoryExist name
--}

doesNameExist :: FilePath -> IO Bool
doesNameExist = fileExist

listMatches :: FilePath -> String -> IO (Either GlobError [String])
listMatches dirName pat = do
  dirName' <- if null dirName
              then getCurrentDirectory
              else return dirName
  handle ((const (return (Right[])))
          :: IOException -> IO (Either GlobError [FilePath])) $ do
    names <- getDirectoryContents dirName'
    let names' = if isHidden pat
                 then filter isHidden names
                 else filter (not . isHidden) names
    return ((matchesGlob pat platformCaseSensitivity) >>= \x->
             Right (filter x names'))

isHidden :: FilePath -> Bool
isHidden ('.':_) = True
isHidden _ = False
listPlain :: FilePath -> String -> IO (Either GlobError [String])
listPlain dirName baseName = do
  exists <- if null baseName
            then doesDirectoryExist dirName
            else doesNameExist (dirName </> baseName)
  return (if exists then Right [baseName] else Right [])

platformCaseSensitivity :: CaseSensitivity
platformCaseSensitivity = if isWindows
                          then IsNotCaseSensitive
                          else IsCaseSensitive

isWindows :: Bool
isWindows = pathSeparator == '\\'

