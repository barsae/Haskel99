
($$) = flip ($)

data Encoded a = Multiple Int a | Single a
    deriving (Eq, Show)

encode :: (Eq a) => [a] -> [Encoded a]
encode [] = []
encode list = map (Multiple 1) list
              $$ foldl next []
              $$ reverse
              $$ map simplify
              where simplify (Multiple 1 a) = Single a
                    simplify a = a
                    next [] x = [x]
                    next (Multiple prevCount prevValue:as) (Multiple thisCount thisValue)
                        | prevValue == thisValue = (Multiple (prevCount + 1) prevValue:as) 
                        | otherwise = (Multiple thisCount thisValue:Multiple prevCount prevValue:as)

