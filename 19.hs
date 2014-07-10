
rotate :: [a] -> Int -> [a]
rotate xs n =
    let index = n `mod` length xs
        (front, back) = splitAt index xs
    in back ++ front
          

