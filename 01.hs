
myLast :: [a] -> a
myLast [] = error "Can't call myLast on empty list"
myLast (x:[]) = x
myLast (_:xs) = myLast xs

