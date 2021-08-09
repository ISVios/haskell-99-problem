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
-- Problem 8
compress :: Eq a => [a] -> [a]
compress [] = []
compress lst@(x:_) = x : compress t where
  (_,t) = span (==x) lst
--
-- Problem 9
pack :: Eq a => [a] -> [[a]]
pack []        = []
pack lst@(x:_) = h : pack t where
  (h,t) = span (==x) lst
--
-- Problem 10
encode :: Eq a => [a] -> [(Int,a)]
encode []         = []
encode lst@(x:xs) = (count, x) : encode noXList where
  count   = length [z | z <- lst, z == x]
  noXList = [y | y <- lst, y /= x]
--
-- Problem 11
data Code a = Single a | Multiple Int a deriving Show
--
encodeModified :: Eq a => [a] -> [Code a]
encodeModified [] = []
encodeModified lst@(x:_) | counter == 1 = Single x           : encodeModified t
                         | otherwise    = Multiple counter x : encodeModified t
  where
    counter = length h
    (h,t)   = span (x==) lst 
--
-- Problem 12
decodeModified :: [Code a] -> [a]
decodeModified []                  = []
decodeModified ((Single a):xs)     = [a] ++ decodeModified xs
decodeModified ((Multiple c a):xs) = nA  ++ decodeModified xs
  where
    nA = take c $ repeat a
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
