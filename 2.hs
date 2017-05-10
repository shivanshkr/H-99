import System.Random

-----------------Problem 11------------------------
data Newlist = Single Char | Multiple Int Char  deriving (Show)

pack :: [Char] -> [[Char]]
pack []         = [[]]
pack [x]        = [[x]]
pack ys@(x:y:xs)
    | x==y      = words ( unwords [[x]] ++ unwords ( pack (y:xs)) )
    | otherwise = [[x]] ++ pack (y:xs)

encodeModified :: [Char] -> [Newlist]
encodeModified [] = error "This is an empty list dude."
encodeModified xs = [if length x == 1 then Single (head x) else Multiple (length x) (head x)| x <- (pack xs)]

----------------Problem 12-------------------------
decodeModified :: [Newlist] -> [Char]
decodeModified []     = []
decodeModified ((Single x) : xs ) = [x] ++ decodeModified xs
decodeModified ((Multiple b x) : xs ) = replicate b x ++ decodeModified xs

----------------Problem 13 --------------------------
--------Same as Problem 11---------------------------

-----------------Problem 14 -------------------------
dupli :: [a] -> [a]
dupli [] = []
dupli (x:xs) = [x,x] ++ dupli xs

----------------Problem 15---------------------------
repli :: [a] -> Int -> [a]
repli [] _     = []
repli _  0     = []
repli (x:xs) n = [x] ++ repli (x:xs) (n-1) ++ repli (xs) (n)

----------------Problem 16--------------------------
dropEvery :: [a] -> Int -> [a]
dropEvery xs  0  = error "dude"
dropEvery [] _  = []
dropEvery _ 1 = []
dropEvery y n = take (n-1) y ++ dropEvery (drop n y) n

-----------------Problem 17--------------------------
split :: [a] -> Int -> ([a],[a])
split [] _ = error "it's empty list..."
split xs n = ((take n xs) , (drop n xs))

-----------------Problem 18------------------------
slice :: [a] -> Int -> Int -> [a]
slice [] _ _ = []
slice xs a b
      | b <  a    = error "what is this man."
      | otherwise = drop (a-1) (take b xs)

----------------Problem 19------------------------
rotate :: [a] -> Int -> [a]
rotate [] _ = []
rotate xs n
    | n >= 0 = drop n $ xs ++ (take n xs)
    | otherwise = take (length xs) $ (drop ((length xs) + n) xs) ++ xs

---------------Problem 20 --------------------------
removeAt :: Int -> [a] -> (a,[a])
removeAt _ [] = error "empty"
removeAt n xs
  | length xs < n = error "enter large list"
  | otherwise = ( ( xs !! (n-1)), (remain n xs) )

remain :: Int -> [a] -> [a]
remain n xs = ( take (n-1) xs ) ++ (drop n xs)

---------------Problem 21--------------------------
insertAt :: a -> [a] -> Int -> [a]
insertAt x xs n
    | n < 0             = error "hahaha"
    | length xs < (n-1) = error "hahahahaha"
    | otherwise         = (take (n-1) xs) ++ [x] ++ (drop (n-1) xs)

--------------Problem 22----------------------------
range :: Int -> Int -> [Int]
range n m
      | n > m     = error "ldskf"
      | n == m    = [m]
      | otherwise = n : (range (n+1) m)

--------------Problem 23--------------------------
--rnd_select :: [a] -> Int -> [a]
--rnd_select xs n = random
-------------Problem 24--------------------------
diff_select :: Int -> Int -> [Int]
diff_select 0 _ = []
diff_select n m = take n $ randomRs (1,m) (mkStdGen 100)

------------------
