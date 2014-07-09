
myButLast :: [a] -> a
myButLast [] = error "Can't call myLast on empty list"
myButLast (x:[]) = error "Can't call myLast on single element list"
myButLast (x:y:[]) = x
myButLast (x:xs) = myButLast xs

