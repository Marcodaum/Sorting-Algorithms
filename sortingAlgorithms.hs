-- Mergesort
mergesort :: Ord a => [a] -> [a]
mergesort [] = []
mergesort [a] = [a]
mergesort x = merge (mergesort (take ((length x) `div` 2) x)) (mergesort (drop ((length x) `div` 2) x))
    where
        merge :: Ord a => [a] -> [a] -> [a]
        merge [] x = x
        merge x [] = x
        merge (x:xs) (y:ys) | y > x = x:(merge xs (y:ys))
                            | otherwise = y:(merge (x:xs) ys)


-- Quicksort
quicksort :: Ord a => [a] -> [a]
quicksort [] = []
quicksort (x:xs) = (quicksort smaller) ++ [x] ++ (quicksort bigger)
            where
                smaller = [y |y <- xs, y <= x]
                bigger  = [y | y <- xs, y > x]


-- Insertionsort
insertionsort :: Ord a => [a] -> [a]
insertionsort [] = []
insertionsort (x:xs) = inssort x (insertionsort xs)
    where
        inssort :: Ord a => a -> [a] -> [a]
        inssort a [] = [a]
        inssort x (y:ys) | x <= y = x:y:ys
                         | otherwise = y : (inssort x ys)


-- Binarysort
data Tree = Val Int | Node Tree Tree Tree | Nil
    deriving Show

list2tree :: [Int] -> Tree
list2tree [] = Nil
list2tree (x:xs) = (insertElem (list2tree xs) (Val x))

insertElem :: Tree -> Tree -> Tree
insertElem Nil val = val
insertElem (Val x) (Val y) | x <= y = Node Nil (Val x) (Val y)
                           | otherwise = Node (Val y) (Val x) Nil
insertElem (Node (x) (Val y) (z)) (Val a) | a <= y = Node (insertElem x (Val a)) (Val y) (z)
                                          | a > y = Node (x) (Val y) (insertElem z (Val a))

flatten :: Tree -> [Int]
flatten Nil = []
flatten (Val x) = [x]
flatten (Node (x) (Val y) (z)) = (flatten x) ++ [y] ++ (flatten z)

binarysort :: [Int] -> [Int]
binarysort [] = []
binarysort xs = flatten (list2tree xs)


-- Bubblesort
bubblesort :: Ord a => [a] -> [a]
bubblesort [] = []
bubblesort [x] = [x]
bubblesort xs = loop xs 0
    where 
        loop :: Ord a => [a] -> Int -> [a]
        loop (x:xs) a | a >= (length (x:xs) - 1) = (x:xs)
                      | otherwise = loop (change (x:xs)) (a+1)
            where 
                change :: Ord a => [a] -> [a]
                change [] =  []
                change [a] = [a]
                change (x:y:xs) | x <= y = x:(change (y:xs))
                                | otherwise = y:(change (x:xs))