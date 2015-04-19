module WC where
main :: IO ()
main =  interact wordCount
  where wordCount input = show (length input) ++ "\n"
                          
