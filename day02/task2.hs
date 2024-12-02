getLinesFromFile :: String -> IO [String]
getLinesFromFile file = do
    contents <- readFile file
    let fileLines = lines contents
    return fileLines

formatReport :: String -> [Int]
formatReport [] = []
formatReport string =
    let num = takeWhile (/=' ') string
        remains = length string - length num - 1
        tail = if remains > 0 then reverse $ take remains (reverse string) else []
    in read num : formatReport tail

reportToTuples :: [Int] -> [(Int, Int)]
reportToTuples [] = []
reportToTuples [_] = []
reportToTuples report =
    let a = head report
        t = tail report
    in (a, head t) : reportToTuples t

reportSafe :: [Int] -> Bool
reportSafe report =
    let
        tuples = reportToTuples report
        inc = all (\(a,b) -> a - b < 0) tuples
        dec = all (\(a,b) -> a - b > 0) tuples
        lim = all (\(a,b) -> (abs (a-b) <= 3) && a /= b) tuples
    in ((inc || dec) && lim)

dampenProblems :: [Int] -> Bool
dampenProblems report = any (dampenProblems' report) [0.. length report - 1]
    where
        dampenProblems' :: [Int] -> Int -> Bool
        dampenProblems' rp i = reportSafe (removeNthElement i rp)

removeNthElement :: Int -> [a] -> [a]
removeNthElement _ [] = []
removeNthElement n xs = take n xs ++ drop (n+1) xs

processReport :: [Int] -> Bool
processReport report = reportSafe report || dampenProblems report

main = do
    reports <- getLinesFromFile "input.txt"
    print $ length $ filter id (processReport <$> map formatReport reports)