type AlignmentType = (String, String)

scoreMatch, scoreMismatch, scoreSpace :: Int
scoreMatch = 0
scoreMismatch = -1
scoreSpace = -1

string1 = "writers"
string2 = "vintner"

-- Attaches the first parameter h1 to the first item of every
-- tuple in the list of tuples and attaches the second parameter
-- h2 to the second item of every tuple in the list of tuples.
attachHeads :: a -> a -> [([a],[a])] -> [([a],[a])]
attachHeads h1 h2 aList = [(h1:xs,h2:ys) | (xs,ys) <- aList]

tupleScore :: AlignmentType -> Int
tupleScore (s1, s2) = sum . zipWith score s1 $ s2

-- Get the or every item in the list which has the highest value
-- according to the valueFcn function.
maximaBy :: Ord b => (a -> b) -> [a] -> [a]
maximaBy valueFcn list = map fst maxList
  where funList = map valueFcn list
        maxVal  = maximum funList
        maxList = filter ((== maxVal) . snd) . zip list $ funList

-- Calculate the score between two characters.
score :: Char -> Char -> Int
score x y
  | x == '-' || y == '-' = scoreSpace
  | x == y               = scoreMatch
  | otherwise            = scoreMismatch

-- Calculate the similiarity score of two strings.
-- If they are not of the same length, let it crash.
similarityScoreUnoptimized :: String -> String -> Int
similarityScoreUnoptimized [] []       = 0
similarityScoreUnoptimized (s1:ss1) [] = score s1 '-' + similarityScoreUnoptimized ss1 []
similarityScoreUnoptimized [] (s2:ss2) = score '-' s2 + similarityScoreUnoptimized [] ss2
similarityScoreUnoptimized (s1:ss1) (s2:ss2) =
  maximum [score s1 s2  + similarityScoreUnoptimized ss1 ss2,
           score s1 '-' + similarityScoreUnoptimized ss1 (s2:ss2),
           score '-' s2 + similarityScoreUnoptimized (s1:ss1) ss2]

similarityScore :: String -> String -> Int
similarityScore s1 s2 = sScore (length s1) (length s2)
  where
    sScore :: Int -> Int -> Int
    sScore x y = sTable !! x !! y
    sTable = [[ sEntry x y | y <- [0..] ] | x <- [0..]]

    sEntry :: Int -> Int -> Int
    sEntry 0 0 = 0
    sEntry 0 y = score '-' (s2 !! (y - 1)) + sScore 0 (y - 1)
    sEntry x 0 = score (s1 !! (x - 1)) '-' + sScore (x - 1) 0
    sEntry x y = maximum [score cx cy  + sScore (x - 1) (y - 1),
                          score cx '-' + sScore (x - 1) y,
                          score '-' cy + sScore x (y - 1)]
                            where
                              cx = s1 !! (x - 1)
                              cy = s2 !! (y - 1)

optAlignmentsUnoptimized :: String -> String -> [AlignmentType]
optAlignmentsUnoptimized [] []         = [([], [])]
optAlignmentsUnoptimized (x:xs) []     = attachHeads x '-' $ optAlignmentsUnoptimized xs []
optAlignmentsUnoptimized [] (y:ys)     = attachHeads '-' y $ optAlignmentsUnoptimized [] ys
optAlignmentsUnoptimized (x:xs) (y:ys) = 
  maximaBy tupleScore . concat $ [attachHeads x y   $ optAlignmentsUnoptimized xs ys,
                                  attachHeads x '-' $ optAlignmentsUnoptimized xs (y:ys),
                                  attachHeads '-' y $ optAlignmentsUnoptimized (x:xs) ys]