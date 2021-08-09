module Solution where
-- Problem 1
myLast :: [a] -> a
myLast []     = error "Solution.myLast: list is empty."
myLast (x:[]) = x
myLast (x:xs) = myLast xs
-- 
-- Problem 2
myButLast :: [a] -> a 
myButLast []       = error "Solution.myButLast: list is empty."
myButLast (x:[])   = error "Solution.myButLast: size of list must be >= 2."
myButLast (x:_:[]) = x 
--
-- Problem 3
elementAt :: [a] -> Int -> a
elementAt []     _                     = error "Solution.elementAt: list is empty."
elementAt (x:_)  0                     = x
elementAt (x:xs) i | (succ i) > length (x:xs) = error "Solution.elementAt: index is great than size of list."
                   | i < 0                    = error "Solution.elementAt: index must be >= 0."
                   | otherwise                = elementAt xs (pred i) 
--
-- Problem 4
myLength :: [a] -> Int
myLength []     = 0
myLength (_:xs) = 1 + myLength xs
--
-- Problem 5
myReverse :: [a] -> [a]
myReverse []     = []
myReverse (x:xs) = myReverse xs ++ [x]
--
-- Problem 6
isPalindrome :: Eq a => [a] -> Bool
isPalindrome xs = myReverse xs == xs -- or use Prelude.reverse
--
-- Problem 7
data NestedList a = Elem a | List [NestedList a]
--
flatten :: NestedList a -> [a]
flatten (Elem a)     = [a]
flatten (List [])    = []
flatten (List(x:xs)) = flatten x ++ flatten (List xs)
--
--
--
--
--
--
--
--
--
--
--
--
--
--
--
--
--
--
--
--
--
--
--
--
--
--
--
--
--
--
--
--
--
--
--
--
--
--
--
--
--
--
--
--
--
--
--
--
--
--
--
--
--
--
--
--
--
--
--
--
--
--
--
--
--
--
--
--
--
--
--
--
--
--
--
--
--
--
--
--
--
--
--
--
--
