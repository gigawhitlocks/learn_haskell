show2d :: [[Integer]] -> String
show2d [x] = show x
show2d (x:xs) = show x ++ "\n" ++ show2d xs

multiples :: Integer -> [[Integer]]
multiples x = filter ((1 /=) . (length)) $ 
    map (\y->filter ((0 ==) . (`mod` y)) [1 .. x]) [1 .. x]


data Tree a = EmptyTree | Node a (Tree a) (Tree a) deriving (Show)

singleton :: a -> Tree a
singleton x = Node x EmptyTree EmptyTree

treeInsert :: (Ord a) => a -> Tree a -> Tree a
treeInsert x EmptyTree = singleton x
treeInsert x (Node a left right)
    | x == a = Node x left right
    | x < a = Node a (treeInsert x left) right
    | x > a = Node a left (treeInsert x right)


treeElem :: (Ord a) => a -> Tree a -> Bool
treeElem x EmptyTree = False
treeElem x (Node a left right)
    | x == a = True
    | x < a = treeElem x left
    | x > a = treeElem x right


quickSort :: [Integer] -> [Integer]
quickSort [] = []
quickSort (x:xs) = quickSort ( filter (<= x) xs )
    ++ [x] ++ quickSort (filter (>= x) xs )


