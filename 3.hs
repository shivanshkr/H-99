
---------------Problem 31 -------------------------
isPrime :: Int -> Bool
isPrime n = foldr (\x acc -> if n `mod` x == 0 then (False && acc) else (True && acc) ) True [2..(n-1)]

---------------Problem 32--------------------------
mygcd :: Int -> Int ->  Maybe Int
mygcd x y
    | x <= 0 || y<= 0 = Nothing
    | x == y    = Just x
    | x > y     = mygcd (x-y) y
    | otherwise = mygcd x (y-x)

----------------
position :: (Eq a , Eq b) =>(a,b) -> [(a,b)] -> Int
position _ [] = 0
position (g,h) xs = length ( takeWhile (\(x,y) -> g /= x ||  h/= y )  xs )
