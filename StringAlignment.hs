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

-- Calculate the similiarity score of two strings NOT using memoization.
similarityScoreUnoptimized :: String -> String -> Int
similarityScoreUnoptimized [] []       = 0
similarityScoreUnoptimized (s1:ss1) [] = score s1 '-' + similarityScoreUnoptimized ss1 []
similarityScoreUnoptimized [] (s2:ss2) = score '-' s2 + similarityScoreUnoptimized [] ss2
similarityScoreUnoptimized (s1:ss1) (s2:ss2) =
  maximum [score s1 s2  + similarityScoreUnoptimized ss1 ss2,
           score s1 '-' + similarityScoreUnoptimized ss1 (s2:ss2),
           score '-' s2 + similarityScoreUnoptimized (s1:ss1) ss2]

-- Calculate the similarity score of two strings using memoization.
similarityScore :: String -> String -> Int
similarityScore s1 s2 = sScore (length s1) (length s2)
  where
    sScore :: Int -> Int -> Int
    sScore x y = sTable !! x !! y
    sTable = [[ sEntry x y | y <- [0..] ] | x <- [0..]]

    cx, cy :: Int -> Char
    cx x = s1 !! (x - 1)
    cy y = s2 !! (y - 1)

    sEntry :: Int -> Int -> Int
    sEntry 0 0 = 0
    sEntry x 0 = score (cx x) '-' + sScore (x - 1) 0
    sEntry 0 y = score '-' (cy y) + sScore 0 (y - 1)
    sEntry x y = maximum [score (cx x) (cy y) + sScore (x - 1) (y - 1),
                          score (cx x) '-'    + sScore (x - 1) y,
                          score '-' (cy y)    + sScore x (y - 1)]

-- Calculate the optimal string alignment NOT using memoization.
optAlignmentsUnoptimized :: String -> String -> [AlignmentType]
optAlignmentsUnoptimized [] []         = [([], [])]
optAlignmentsUnoptimized (x:xs) []     = attachHeads x '-' $ optAlignmentsUnoptimized xs []
optAlignmentsUnoptimized [] (y:ys)     = attachHeads '-' y $ optAlignmentsUnoptimized [] ys
optAlignmentsUnoptimized (x:xs) (y:ys) = 
  maximaBy tupleScore . concat $ [attachHeads x y   $ optAlignmentsUnoptimized xs ys,
                                  attachHeads x '-' $ optAlignmentsUnoptimized xs (y:ys),
                                  attachHeads '-' y $ optAlignmentsUnoptimized (x:xs) ys]
    where 
      tupleScore :: AlignmentType -> Int
      tupleScore (s1, s2) = sum . zipWith score s1 $ s2

-- Calculate optimal string alignment using memoization.
optAlignments :: String -> String -> [AlignmentType]
optAlignments s1 s2 = [(reverse r1, reverse r2) | (r1, r2) <- snd . optA (length s1) $ length s2]
  where
    add :: Char -> Char -> (Int, [AlignmentType]) -> (Int, [AlignmentType])
    add h1 h2 (i, a) = ((score h1 h2) + i, attachHeads h1 h2 a)

    cx, cy :: Int -> Char
    cx x = s1 !! (x - 1)
    cy y = s2 !! (y - 1)

    optA :: Int -> Int -> (Int, [AlignmentType])
    optA x y = oTable !! x !! y
    oTable = [[ oEntry x y | y <- [0..]] | x <- [0..]]

    oEntry :: Int -> Int -> (Int, [AlignmentType])
    oEntry 0 0 = (0, [([], [])])
    oEntry x 0 = add (cx x) '-' $ optA (x - 1) 0
    oEntry 0 y = add '-' (cy y) $ optA 0 (y - 1)
    oEntry x y = (fst . head $ res, concat . map snd $ res)
      where res = maximaBy fst $ [add (cx x) (cy y) $ optA (x - 1) (y - 1),
                                  add (cx x) '-'    $ optA (x - 1) y,
                                  add '-' (cy y)    $ optA x (y - 1)]

-- Output all number of optimal aligments between two strings.
outputOptAlignments :: String -> String -> IO ()
outputOptAlignments string1 string2 = do
  let results = optAlignments string1 string2
  putStrLn $ "There are " ++ (show . length $ results) ++ " optimal alignments!"
  mapM_ (\(r1, r2) -> putStrLn $ "\n" ++ r1 ++ "\n" ++ r2) results
  return ()