getLinesFromFile :: String -> IO [String]
getLinesFromFile file = do
    contents <- readFile file
    let fileLines = lines contents
    return fileLines

splitLineToTuple :: String -> (Int, Int)
splitLineToTuple string =
    let first = read $ takeWhile (/= ' ') string
        second = read . reverse $ takeWhile (/= ' ') $ reverse string
    in (first, second)

similarityScore :: [Int] -> Int -> Int
similarityScore test input = input * length (filter (==input) test)

main = do
    lines <- getLinesFromFile "input.txt"
    let numbers = fmap splitLineToTuple lines
    let left = map fst numbers
    let right = map snd numbers
    print . sum $ fmap (similarityScore right) left