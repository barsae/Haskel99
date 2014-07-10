
slice :: [a] -> Int -> Int -> [a]
slice [] _ _ = []
slice _ 0 0 = []
slice (x:xs) 0 end = x : slice xs 0 (end - 1)
slice (x:xs) start end = slice xs (start - 1) (end - 1)

