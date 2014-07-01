
removeUntilNot :: (Eq a) => a -> [a] -> [a]
removeUntilNot y [] = []
removeUntilNot y (x:xs) = if x == y then removeUntilNot y xs else (x:xs)

myCompress :: (Eq a) => [a] -> [a]
myCompress [] = []
myCompress (x:xs) = [x] ++ (myCompress $ removeUntilNot x xs)

