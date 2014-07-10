
powers :: Integer -> [Integer]
powers base = 1 : gen base
    where gen x = x : (gen  (x * base))

