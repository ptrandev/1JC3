{-# LANGUAGE DeriveFunctor,DeriveFoldable #-}

-- Patterns of Computation
-- 1. Generalizing Patterns of Computation
-- 2. Mapping
-- 3. Filter
-- 4. Zip
-- 5. Foldr
-- 6. Applying Patterns to Trees

-- 1. Generalizing Patterns of Computation
-- Generality through polymorphism

-- length' :: [a] -> Int
-- (++)' :: [a] -> [a] -> [a]
-- take' :: Int -> [a] -> [a]

-- Two common patterns of computation
-- 1. Transform every element of a list (mapping)
-- 2. Combine elements of a list (folding)

-- 2. Mapping
-- apply a function to every element in a list

-- defined via recursive
map' :: (a -> b) -> [a] -> [b]
map' f [] = []
map' f (x:xs) = f x : map' f xs

-- defined via list comprehension
map'' :: (a -> b) -> [a] -> [b]
map'' f xs = [f x | x <- xs]

-- map (1+) [1,2,3,4,5]
-- = [2,3,4,5,6]

-- 3. Filter
-- select elements of a list that satisfy a boolean function

-- defined via recursion
filter' :: (a -> Bool) -> [a] -> [a]
filter' p [] = []
filter' p (x:xs) = if p x then x : filter' p xs else filter' p xs

-- defined via list comprehension
filter'' :: (a -> Bool) -> [a] -> [a]
filter'' p xs = [x | x <- xs, p x]

-- filter even [1,2,3,4,5]
-- = filter even [2,3,4,5]
-- = 2 : filter even [3,4,5]
-- = 2 : filter even [4,5]
-- = 2 : 4 : filter even [5]
-- = 2 : 4 : filter even []
-- = 2 : 4 : []
-- = [2,4]

-- 4. Zip
-- combine two lists into a list of pairs, cutting off the longer list

-- zip' :: [a] -> [b] -> [(a,b)]
-- zip ['a','b','c'] [1,2,3,4]
-- = [('a',1),('b',2),('c',3)]

pairs :: [a] -> [(a, a)]
pairs xs = zip xs (tail xs)

-- pairs [1,2,3,4]
-- = zip [1,2,3,4] (tail [1,2,3,4])
-- = zip [1,2,3,4] [2,3,4]
-- = [(1,2), (2,3), (3,4)]

sorted :: Ord a => [a] -> Bool
sorted xs = and [x <= y | (x, y) <- pairs xs]

-- (x, y) <- pairs xs
-- [(1,2), (2,3), (4,4)]
-- [1 <= 2, 2 <= 3, 3 <= 4]
-- [True, True, True]
-- and [True, True, True]
-- True

-- 5. Foldr
-- Fold Right; combines elements of a list from right to left

-- a number of functions on lists can be defined with this simple pattern
-- I'll call it the foldr pattern... we'll define some specific examples first
-- f [] = v
-- f (x:xs) = x # f xs
-- v is the value of the empty list, like 0 or 1; called the identity value;
-- the base case that terminates the recursion and doesn't depend on the list
-- where # is some binary operator, like + or *, etc.

sum' :: Num a => [a] -> a
sum' [] = 0 -- v is 0
sum' (x:xs) = x + sum' xs -- # is +

-- sum' [1,2,3]
-- = 1 + sum' [2,3]
-- = 1 + 2 + sum' [3]
-- = 1 + 2 + 3 + sum' []
-- = 1 + 2 + 3 + 0
-- = 6

product' :: Num a => [a] -> a
product' [] = 1 -- v is 1
product' (x:xs) = x * product' xs -- # is *

-- product [2,3,4]
-- 2 * product [3,4]
-- 2 * 3 * product [4]
-- 2 * 3 * 4 * product []
-- 2 * 3 * 4 * 1
-- 24

and' :: [Bool] -> Bool
and' [] = True
and' (x:xs) = x && and' xs

-- and [True, True]
-- = True && and [True]
-- = True && True && and []
-- = True && True && True
-- = True

-- and [True, False]
-- = True && False && and []
-- = True && False && True
-- = False

-- the higher order library function foldr encapsulates this pattern
-- with the function (#) and value (v) as arguments

sum'' :: Num a => [a] -> a
sum'' = foldr (+) 0

product'' :: Num a => [a] -> a
product'' = foldr (*) 1

and'' :: [Bool] -> Bool
and'' = foldr (&&) True

foldr' :: (a -> b -> b) -> b -> [a] -> b
foldr' f v [] = v
foldr' f v (x:xs) = f x (foldr' f v xs)

-- foldr (+) 0 [1,2,3]
-- 1 + foldr (+) 0 [2,3]
-- 1 + 2 + foldr (+) 0 [3]
-- 1 + 2 + 3 + foldr (+) 0 []
-- 1 + 2 + 3 + 0
-- = 6

-- lambda notation refresher
-- \x y ->
-- where the argument x is the current element of the list
-- and the argument y is the result of the recursive call

length' :: [a] -> Int
length' = foldr (\_ n -> 1 + n) 0

-- length [1,2,3]
-- = (\_ n -> 1 + n) 1 (foldr (\_ n -> 1 + n) 0 [2,3])
-- = 1 + foldr (\_ n -> 1 + n) 0 [2,3]
-- = 1 + (\_ n -> 1 + n) 2 (foldr (\_ n -> 1 + n) 0 [3])
-- = 1 + 1 + foldr (\_ n -> 1 + n) 0 [3]
-- = 1 + 1 + (\_ n -> 1 + n) 3 (foldr (\_ n -> 1 + n) 0 [])
-- = 1 + 1 + 1 + foldr (\_ n -> 1 + n) 0 []
-- = 1 + 1 + 1 + 0
-- = 3

reverse' :: [a] -> [a]
reverse' = foldr (\x xs -> xs ++ [x]) []

data BinTree a = Node (BinTree a) (BinTree a) a -- left, right, value
                | Leaf a -- value
  deriving (Show, Eq, Functor, Foldable) -- Functor and Foldable instances, will be explained later
exampleTree = Node (Node (Leaf 5)
                         (Leaf 6) 7)
                   (Leaf 3)
                   8

{-
       8
      / \
    7    3
  / \
 5   6
-}

binTreeMap :: (a -> b) -> BinTree a -> BinTree b
binTreeMap f (Leaf x) = Leaf (f x)
binTreeMap f (Node l r x) = Node (binTreeMap f l) (binTreeMap f r) (f x)

-- binTreeMap (1+) exampleTree
-- = Node (Node (Leaf 5) (Leaf 6) 7) (Leaf 3) 8
-- = Node (Node (Leaf 1+5) (Leaf 1+6) 1+7) (Leaf 1+3) 1+8
-- = Node (Node (Leaf 6) (Leaf 7) 8) (Leaf 4) 9

-- f [] = v
-- f (x:xs) = x # f xs

{-
       8
      / \
    7    3
  / \
 5   6
-}

binTreeFold :: (a -> b -> b) -> b -> BinTree a -> b
binTreeFold op v (Leaf x) = x `op` v
binTreeFold op v (Node l r x) =
  let
    v' = x `op` v''
    v'' = binTreeFold op v l
  in
    binTreeFold op v' r

-- binTreeFold (+) 0 exampleTree
-- = 8 + 7 + 6 + 5 + 3
-- = 29

{-
    7
  / \
 5   6
-}

-- binTreeFold (+) 0 Tree2
-- v'' = 5 + 0 = 5
-- v' = 7 + 5 = 12
-- 6 + 12 = 18