myLast :: [a] -> a
myLast []     = error "The list is empty."
myLast [a]    = a
myLast (x:xs) = myLast xs



myButLast :: [a] -> a
myButLast []     = error "The list is empty."
myButLast [x]    = error "The list has only one element."
myButLast [a,b]  = a
myButLast (x:xs) = myButLast (xs)

elementAt :: [a] -> Int -> a
elementAt [] _     = error "The list is empty."
elementAt _  0     = error "Can't find this one."
elementAt (x:_) 1  = x
elementAt (x:xs) i = elementAt (xs) (i-1)

myLength :: [a] -> Int
myLength []     = 0
myLength (x:xs) = 1 + myLength (xs)

myReverse :: [a] -> [a]
myReverse []     = []
myReverse (x:xs) = myReverse (xs) ++ [x]

isPalindrome :: Eq a => [a] -> Bool
isPalindrome xs
      | xs == myReverse (xs) = True
      | otherwise            = False

--7--data NestedList a = Elem a | List [NestedList a]
--7--flatten :: NestedList a ->  [a]
--7--flatten ( List [] )   = []
--7--flatten ( Elem x )    = [ x ]
--7--flatten ( List (x:xs) ) = [x] ++ flatten xs

--8--
compress :: Eq a => [a] -> [a]
compress [] = []
compress [x] = [x]
compress (x:y:xs)
    | x == y    = compress (y:xs)
    | otherwise = [x] ++ compress (y:xs)

--9--
pack :: [Char] -> [[Char]]
pack []         = [[]]
pack [x]        = [[x]]
pack ys@(x:y:xs)
    | x==y      = words ( unwords [[x]] ++ unwords ( pack (y:xs)) )
    | otherwise = [[x]] ++ pack (y:xs)

--10--
encode :: [Char] -> [(Int,Char)]
encode []  = error "This is an empty list dude...."
encode xs  = [ (length x ,head x) | x <- (pack xs)]
