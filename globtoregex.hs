module GlobRegex
       (
         globToRegex
       , matchesGlob
       , namesMatching
       ) where

import Text.Regex.Posix ((=~))
import Data.Char
import System.Directory (doesDirectoryExist, doesFileExist,
                         getCurrentDirectory, getDirectoryContents)
import System.FilePath (
  dropTrailingPathSeparator,
  splitFileName,
  (</>),
  pathSeparator)

import Control.Exception (handle, IOException)
import Control.Monad (forM)

data CaseSensitivity = IsCaseSensitive | IsNotCaseSensitive

globToRegex :: CaseSensitivity->String->String
globToRegex s cs = '^' : globToRegex' s cs ++ "$"
        
globToRegex' :: CaseSensitivity -> String -> String
globToRegex' _ "" = ""

globToRegex' s ('*':cs) = ".*" ++ globToRegex' s cs

globToRegex' s ('?':cs) = "." ++ globToRegex' s cs

globToRegex' s ('[':'!':c:cs) = "[^" ++ uplowhead s (c : charClass s cs)
globToRegex' s ('[':c:cs) = "[" ++ uplowhead s (c : charClass s cs)
globToRegex' _ ('[':_) = error "unterminated character class"

globToRegex' s (c:cs) = escape s c ++ globToRegex' s cs

escape :: CaseSensitivity -> Char -> String
escape s c | c `elem` regexChars = '\\' : [c]
           | otherwise = uplowheadCharClass s [c]
  where regexChars = "\\+()^$.{}]|"

charClass :: CaseSensitivity -> String -> String
charClass s (']':cs) = ']' : globToRegex' s cs
charClass s (c:cs) = uplowhead s (c : charClass s cs)
charClass _ [] = error "unterminated character class"

uplowheadCharClass :: CaseSensitivity->[Char]->[Char]
uplowheadCharClass _ [] = []
uplowheadCharClass IsNotCaseSensitive (c:cs) = '[':(toLower c):(toUpper c):']':cs
uplowheadCharClass IsCaseSensitive cs = cs

uplowhead :: CaseSensitivity->[Char]->[Char]
uplowhead _ [] = []
uplowhead IsNotCaseSensitive (c:cs) = (toLower c):(toUpper c):cs
uplowhead IsCaseSensitive cs = cs

matchesGlob :: String -> CaseSensitivity -> FilePath -> Bool
matchesGlob pat s name = name =~ globToRegex s pat

isPattern :: String -> Bool
isPattern = any (`elem` "[*?")


namesMatching pat
  | not (isPattern pat) = do
      exists <- doesNameExist pat
      return (if exists then [pat] else [])
  | otherwise = do
      case splitFileName pat of
       ("", baseName) -> do
         curDir <- getCurrentDirectory
         listMatches curDir baseName
       (dirName, baseName) -> do
         dirs <- if isPattern dirName
                 then namesMatching (dropTrailingPathSeparator dirName)
                 else return [dirName]
         let listDir = if isPattern baseName
                       then listMatches
                       else listPlain
         pathNames <- forM dirs $ \dir -> do
           baseNames <- listDir dir baseName
           return (map (dir </>) baseNames)
         return (concat pathNames)

doesNameExist :: FilePath -> IO Bool
doesNameExist name = do
  fileExists <- doesFileExist name
  if fileExists
    then return True
    else doesDirectoryExist name

listMatches :: FilePath -> String -> IO [String]
listMatches dirName pat = do
  dirName' <- if null dirName
              then getCurrentDirectory
              else return dirName
  handle ((const (return [])) :: IOException -> IO [FilePath]) $ do
    names <- getDirectoryContents dirName'
    let names' = if isHidden pat
                 then filter isHidden names
                 else filter (not . isHidden) names
    return (filter (matchesGlob pat platformCaseSensitivity) names')

isHidden :: FilePath -> Bool
isHidden ('.':_) = True
isHidden _ = False

listPlain :: FilePath -> String -> IO [String]
listPlain dirName baseName = do
  exists <- if null baseName
            then doesDirectoryExist dirName
            else doesNameExist (dirName </> baseName)
  return (if exists then [baseName] else [])

platformCaseSensitivity :: CaseSensitivity
platformCaseSensitivity = if isWindows
                          then IsNotCaseSensitive
                          else IsCaseSensitive

isWindows :: Bool
isWindows = pathSeparator == '\\'
