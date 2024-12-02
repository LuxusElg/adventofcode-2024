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

reportSafe :: [(Int, Int)] -> Bool
reportSafe report =
    let inc = all (\(a,b) -> a - b < 0) report
        dec = all (\(a,b) -> a - b > 0) report
        lim = all (\(a,b) -> (abs (a-b) <= 3) && a /= b) report
    in ((inc || dec) && lim)

main = do
    reports <- getLinesFromFile "input.txt"
    print $ length $ filter id (map reportSafe (reportToTuples <$> map formatReport reports))