
split :: [a] -> Int -> ([a], [a])
split xs 0 = ([], xs)
split (x:xs) n = let (front, back) = split xs (n - 1)
                 in (x : front, back)


