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
-- Problem 13
encodeDirect :: Eq a => [a] -> [Code a]
encodeDirect = encodeModified  -- IDK what need to do.
--
-- Problem 14
dupli :: [a] -> [a] 
dupli = concat . map (\x -> [x,x])
--
-- Problem 15
repli :: [a] -> Int -> [a]
repli [] _     = []
repli xs count = concat $ map (take count . repeat) xs
--
-- Problem 16
dropEvery :: [a] -> Int -> [a]
dropEvery xs ind = [z | (i,z) <- zip [0..] xs, ind /= i]
--
-- Problem 17
split :: [a] -> Int -> ([a], [a])
split xs i = (take i xs, drop i xs)
--
-- Problem 18
slice :: [a] -> Int -> Int -> [a]
slice xs s e = take (e - (pred s)) $ drop (pred s) xs
--
-- Problem 19
rotate :: [a] -> Int -> [a]
rotate []  _         = []
rotate xs  0         = xs
rotate lst i | i > 0 = (drop i lst) ++ (take i lst)
             | i < 0 = rotate lst (length lst + i)
--
-- Problem 20
remoteAt :: Int -> [a] -> (a, [a])
remoteAt ind xs = (xs !! ind, [z | (i,z) <- zip [0..] xs, i /= ind]) 
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
