
pack :: (Eq a) => [a] -> [[a]]
pack [] = []
pack (x:xs) = let front = takeWhile (== x) (x:xs)
                  back  = dropWhile (== x) (x:xs)
               in front : (pack back)

encode :: (Eq a) => [a] -> [(Int, a)]
encode list = let packed = pack list
              in zip (map length packed) (map head packed)
 
