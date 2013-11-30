scoreMatch, scoreMismatch, scoreSpace :: Int
scoreMatch = 1
scoreMismatch = -1
scoreSpace = -2

similarityScore :: String -> String -> Int
similarityScore [] [] = 0
similarityScore (s1:ss1) (s2: ss2) = score + similarityScore ss1 ss2
  where score
            | s1 == s2               = scoreMatch
            | s1 == '-' || s2 == '-' = scoreSpace
            | otherwise              = scoreMismatch