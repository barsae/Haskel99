
takeWhile' :: (a -> Bool) -> [a] -> [a]
takeWhile' pred [] = []
takeWhile' pred (x:xs)
    | pred x = x : (takeWhile' pred xs)
    | otherwise = []

skipWhile' :: (a -> Bool) -> [a] -> [a]
skipWhile' pred [] = []
skipWhile' pred (x:xs)
    | pred x = skipWhile' pred xs
    | otherwise = (x:xs)

pack' :: (Eq a) => [a] -> [[a]]
pack' [] = []
pack' (x:xs) = let front = takeWhile' (== x) (x:xs)
                   back  = skipWhile' (== x) (x:xs)
               in front : (pack' back)

