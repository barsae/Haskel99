
dropEvery :: [a] -> Int -> [a]
dropEvery list n = dropEvery' list n
                   where dropEvery' [] _ = []
                         dropEvery' (x:xs) 1 = dropEvery' xs n
                         dropEvery' (x:xs) c = (x : dropEvery' xs (c - 1))

