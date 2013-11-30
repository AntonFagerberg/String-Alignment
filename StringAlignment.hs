scoreMatch, scoreMismatch, scoreSpace :: Int
scoreMatch = 1
scoreMismatch = -1
scoreSpace = -2

-- Calculate the similiarity score of two strings.
-- If they are not of the same length, let it crash.
similarityScore :: String -> String -> Int
similarityScore [] [] = 0
similarityScore (s1:ss1) (s2: ss2) = score + similarityScore ss1 ss2
  where score
            | s1 == s2               = scoreMatch
            | s1 == '-' || s2 == '-' = scoreSpace
            | otherwise              = scoreMismatch

-- Attaches the first parameter h1 to the first item of every
-- tuple in the list of tuples and attaches the second parameter
-- h2 to the second item of every tuple in the list of tuples.
attachHeads :: a -> a -> [([a],[a])] -> [([a],[a])]
attachHeads h1 h2 aList = [(h1:xs,h2:ys) | (xs,ys) <- aList]

-- Get the or every item in the list which has the highest value
-- according to the valueFcn function.
maximaBy :: Ord b => (a -> b) -> [a] -> [a]
maximaBy valueFcn list = map fst maxList
  where funList = map valueFcn list
        maxVal  = maximum funList
        maxList = filter ((== maxVal) . snd) . zip list $ funList