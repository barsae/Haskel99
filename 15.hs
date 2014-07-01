
singleRepli :: Integer -> a -> [a]
singleRepli 0 x = []
singleRepli count x = x : singleRepli (count - 1) x

repli :: Integer -> [a] -> [a]
repli count list = concatMap (singleRepli count) list

