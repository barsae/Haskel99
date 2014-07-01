
myReverse :: [a] -> [a]
myReverse [] = []
myReverse (x:xs) = (myReverse xs) ++ [x]

myPalindrome :: (Eq a) => [a] -> Bool
myPalindrome x = x == (myReverse x)

