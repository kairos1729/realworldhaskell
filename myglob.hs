module MyGlob(myglob, testmyglob) where

testmyglob :: IO ()
testmyglob = do
  test "*.hs" ".hs" True
  test "*.hs" "hs" False
  test "*.hs" "blah.hs" True
  test "*.hs" "blah.ps" False
  test "*.hs*p" "blah.hsblahp" True
  test "*.hs*p" "blah.hsblah" False
  test "*.hs*p" "blah.psblahp" False
  test "*.hs*" "blah.hs" True
  test "*.hs*" "blah.hsp" True
  test "*.hs*" "blah.ps" False
  test "\\*.hs*" "blah.hs" False
  test "\\*.hs*" "*.hs" True
  test "\\\\*.hs*" "*.hs" False
  test "\\\\*.hs*" "\\bloo.hs" True
  test "h*s*e*l" "haskell" True
  test "h*s*e*l" "haaaaskkkkell" True
  test "h*s*e*l" "haaaaskkkkelly" False
  where
    test p s e = putStrLn $
                 show p ++ " " ++ show s ++ " " ++ show e ++ ": " ++ passFail
      where passFail = if (myglob p s == e) then "PASS" else "FAIL <<<<---" 

myglob :: String->String->Bool
myglob [] [] = True
myglob [] _ = False
myglob ('*':[]) _ =  True
myglob _ [] = False
myglob ('\\':p:ps) (s:ss) | p == s = myglob ps ss
                          | otherwise = False
myglob ('*':ps) (s:ss) | myglob ps (s:ss) = True
                       | otherwise = myglob ('*':ps) ss
myglob (p:ps) (s:ss) | p == s = myglob ps ss                                     
                     | otherwise = False
