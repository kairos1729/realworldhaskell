import Control.Exception
import System.IO

main :: IO()
main = do
  putStrLn "Enter a filename: "
  path <- getLine
  if (path == "exit")
    then return()
    else do
    size <- getFileSize path
    putStrLn $ "Size=" ++ show size
    main

getFileSize :: String -> IO (Maybe Integer)
getFileSize path = handle handleException $
                   bracket (doOpen path) doCleanup getSize

{- Can't really be done:
getFileSizeSwap :: String -> IO (Maybe Integer)
getFileSizeSwap path = bracket
                       (doHandle $ doOpen path)
                       (doHandle $ doCleanup)
                       (doHandle getSize)                       
  where
    doHandle :: IO a
    doHandle = handle handleException
-}

handleException :: IOException -> IO (Maybe Integer)
handleException e = do
  putStrLn $ "Error: " ++ show e
  return Nothing

doOpen :: FilePath -> IO Handle
doOpen path = openFile path ReadMode

doCleanup :: Handle -> IO ()
doCleanup h = do
  putStrLn $ "Cleaning up: " ++ show h
  hClose h
  putStrLn "done."

getSize :: Handle -> IO (Maybe Integer)
getSize h = do
  size <- hFileSize h
  return (Just size)

    
