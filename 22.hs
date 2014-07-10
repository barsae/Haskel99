
range :: Eq a => Enum a => a -> a -> [a]
range start end
    | start == end = [end]
    | otherwise = start : range (succ start) end
                  
