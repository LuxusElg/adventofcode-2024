getLinesFromFile :: String -> IO [String]
getLinesFromFile file = do
    contents <- readFile file
    let fileLines = lines contents
    return fileLines

quicksort :: Ord a => [a] -> [a]
quicksort []     = []
quicksort (p:xs) = (quicksort lesser) ++ [p] ++ (quicksort greater)
    where
        lesser  = filter (< p) xs
        greater = filter (>= p) xs

splitLineToTuple :: String -> (Int, Int)
splitLineToTuple string =
    let first = read $ takeWhile (/= ' ') string
        second = read . reverse $ takeWhile (/= ' ') $ reverse string
    in (first, second)

main = do
    lines <- getLinesFromFile "input.txt"
    let numbers = fmap splitLineToTuple lines
    let left = quicksort $ map fst numbers
    let right = quicksort $ map snd numbers
    print $ sum $ zipWith (\a b -> abs (a - b)) left right