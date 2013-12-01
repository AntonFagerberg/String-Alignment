scoreMatch, scoreMismatch, scoreSpace :: Int
scoreMatch = 0
scoreMismatch = -1
scoreSpace = -1

string1 = "writers"
string2 = "vintner"

-- Calculate the score between two characters.
score :: (Char, Char) -> Int
score (x, y)
  | x == '-' || y == '-' = scoreSpace
  | x == y               = scoreMatch
  | otherwise            = scoreMismatch

-- Calculate the similiarity score of two strings.
-- If they are not of the same length, let it crash.
similarityScoreUnoptimized :: String -> String -> Int
similarityScoreUnoptimized [] []       = 0
similarityScoreUnoptimized (s1:ss1) [] = score (s1, '-') + similarityScoreUnoptimized ss1 []
similarityScoreUnoptimized [] (s2:ss2) = score ('-', s2) + similarityScoreUnoptimized [] ss2
similarityScoreUnoptimized (s1:ss1) (s2:ss2) =
  maximum [score (s1, s2)  + similarityScoreUnoptimized ss1 ss2,
           score (s1, '-') + similarityScoreUnoptimized ss1 (s2:ss2),
           score ('-', s2) + similarityScoreUnoptimized (s1:ss1) ss2]