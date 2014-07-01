import Control.Exception
import Control.Monad

myButLast :: [a] -> a
myButLast [] = error "Can't call myLast on empty list"
myButLast (x:[]) = error "Can't call myLast on single element list"
myButLast (x:y:[]) = x
myButLast (x:xs) = myButLast xs

main :: IO ()
main = do
    assertException (evaluate $ myButLast [])
    assertException (evaluate $ myButLast [1])
    assert (myButLast [1, 2, 3]) 2
    assert (myButLast [1, 2, 3, 4]) 3

