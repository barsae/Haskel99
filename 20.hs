
removeAt :: [a] -> Int -> (a, [a])
removeAt (x:xs) 0 = (x, xs)
removeAt (x:xs) n =
    let (removed, residual) = removeAt xs (n - 1)
    in (removed, x : residual)

