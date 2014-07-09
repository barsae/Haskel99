
($$) = flip ($)

data Encoded a = Multiple Int a | Single a deriving Show

pack :: (Eq a) => [a] -> [[a]]
pack [] = []
pack (x:xs) = let front = takeWhile (== x) (x:xs)
                  back  = dropWhile (== x) (x:xs)
               in front : (pack back)

replaceSingleton :: Encoded a => Encoded a
replaceSingleton (Multiple 1 a) = Single a
replaceSingleton a = a

encode :: (Eq a) => [a] -> [Encoded a]
encode list = let packed = pack list
              in zipWith Multiple (map length packed) (map head packed)
              $$ map replaceSingleton
 
decode :: [Encoded a] -> [a]
decode encoded = concatMap expand encoded
                 where expand (Multiple count a) = replicate count a
                       expand (Single a) = [a]

