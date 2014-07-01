
myIndex :: (Num b) => (Ord b) => [a] -> b -> a
myIndex [] index = error "Index is out of bounds"
myIndex (x:xs) index 
    | index < 0 = error "Index cannot be negative"
    | index == 0 = x
    | otherwise = myIndex xs (index - 1)

