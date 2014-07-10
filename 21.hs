
insertAt :: a -> [a] -> Int -> [a]
insertAt item xs 0 = item : xs
insertAt item (x:xs) index = x : insertAt item xs (index - 1)

