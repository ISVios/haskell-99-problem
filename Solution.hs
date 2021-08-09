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
--
--
--
--
--
--
--
--
