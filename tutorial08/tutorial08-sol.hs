{-# LANGUAGE DeriveFunctor,DeriveFoldable #-}

-- Patterns of Computation
-- Combine patterns together to solve problems

-- 1. Generalizing Patterns of Computation
-- Generality through polymorphism

-- length' :: [a] -> Int
-- (++)' :: [a] -> [a] -> [a]
-- take' :: Int -> [a] -> [a]

-- all of these functions operate on all lists regardless of their type
-- this makes it possible to write functions that are more general
-- and can be used in a wider variety of situations

-- Two common patterns of computation
-- 1. Transform every element of a list (mapping)
-- 2. Combine elements of a list (folding)

-- 2. Mapping
-- apply a function to every element in a list

-- defined via recursion
map' :: (a -> b) -> [a] -> [b]
map' f [] = []
map' f (x:xs) = f x : map' f xs


-- defined via list comprehension
map'' :: (t -> a) -> [t] -> [a]
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
-- = [2,4]

-- 4. Zip
-- combine two lists into a list of pairs, cutting off the longer list

-- zip' :: [a] -> [b] -> [(a,b)]

-- zip ['a','b','c'] [1,2,3,4]
-- = [('a',1),('b',2),('c',3)]

-- We can use zip to create a function that returns a list of adjacent pairs
pairs :: [a] -> [(a,a)]
pairs xs = zip xs (tail xs)

-- pairs [1,2,3,4]
-- = zip [1,2,3,4] (tail [1,2,3,4])
-- = zip [1,2,3,4] [2,3,4]
-- = [(1,2),(2,3),(3,4)]

-- We can now use pairs to create a function that checks if a list is sorted
sorted :: Ord a => [a] -> Bool
sorted xs = and [x <= y | (x,y) <- pairs xs]

-- sorted [1,2,3,4]
-- = and [1 <= 2, 2 <= 3, 3 <= 4]
-- = and [True, True, True]
-- = True

-- 5. Foldr
-- Fold Right; combines elements of a list from right to left

-- a number of functions on lists can be defined with this simple pattern
-- I'll call it the foldr pattern... we'll define some specific examples first
-- f [] = v
-- f (x:xs) = x # f xs
-- where # is some binary operator, like + or *, etc.
-- v is the value of the empty list, like 0 or 1; called the identity value;
-- the base case that terminates the recursion and doesn't depend on the list

-- functions defined using this pattern
sum' :: Num a => [a] -> a
sum' [] = 0 -- v is 0
sum' (x:xs) = x + sum' xs -- # is +

product' :: Num a => [a] -> a
product' [] = 1 -- v is 1
product' (x:xs) = x * product' xs -- # is *

and' :: [Bool] -> Bool
and' [] = True -- v is True
and' (x:xs) = x && and' xs -- # is &&

-- the higher order library function foldr encapsulates this pattern
-- with the function (#) and value (v) as arguments

sum'' :: Num a => [a] -> a
sum'' = foldr (+) 0

product'' :: Num a => [a] -> a
product'' = foldr (*) 1

and'' :: [Bool] -> Bool
and'' = foldr (&&) True

or'' :: [Bool] -> Bool
or'' = foldr (||) False

-- the more generic way to define foldr
-- defined via recursion
foldr' :: (a -> b -> b) -> b -> [a] -> b
foldr' f v [] = v
foldr' f v (x:xs) = f x (foldr' f v xs)

-- a function for summing the elements of a list
-- foldr (+) 0 [1,2,3]
-- = 1 + foldr (+) 0 [2,3]
-- = 1 + (2 + foldr (+) 0 [3])
-- = 1 + (2 + (3 + foldr (+) 0 []))
-- = 1 + (2 + (3 + 0))
-- = 6

-- lambda notation refresher
-- \x y ->
-- where the argument x is the current element of the list
-- and the argument y is the result of the recursive call

length' :: [a] -> Int
length' = foldr (\_ n -> 1 + n) 0 -- first argument of lambda is ignored

-- length' [1,2,3]
-- = foldr (\_ n -> 1 + n) 0 [1,2,3]
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

-- reverse' [1,2,3]
-- = foldr (\x xs -> xs ++ [x]) [] [1,2,3]
-- = (\x xs -> xs ++ [x]) 1 (foldr (\x xs -> xs ++ [x]) [] [2,3])
-- = [] ++ [1]
-- = [1] ++ foldr (\x xs -> xs ++ [x]) [] [2,3]
-- = [1] ++ (\x xs -> xs ++ [x]) 2 (foldr (\x xs -> xs ++ [x]) [] [3])
-- = [1] ++ [] ++ [2]
-- = [1,2] ++ foldr (\x xs -> xs ++ [x]) [] [3]
-- = [1,2] ++ (\x xs -> xs ++ [x]) 3 (foldr (\x xs -> xs ++ [x]) [] [])
-- = [1,2] ++ [] ++ [3]
-- = [1,2,3]

-- 6. Applying Patterns of Computation to Trees

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
-- = Node (Node (Leaf 6) (Leaf 7) 8) (Leaf 4) 9
-- = Node (Node (Leaf (1+6)) (Leaf (1+7)) (1+8)) (Leaf (1+4))
-- = Node (Node (Leaf 7) (Leaf 8) 9) (Leaf 5)

binTreeFold :: (a -> b -> b) -> b -> BinTree a -> b
binTreeFold op v (Leaf x) = x `op` v
binTreeFold op v (Node l r x) =
  let
    v' = x `op` v'' -- combine current node with result of folding left subtree
    v'' = binTreeFold op v l -- fold left subtree
  in
    binTreeFold op v' r -- fold right subtree

-- 5 + 6 = 11
-- 11 + 7 = 18
-- 18 + 3 = 21
-- 21 + 8 = 29

-- binTreeFold (+) 0 exampleTree
-- = 29
-- = 8 + 7 + 6 + 5 + 3

-- Added Functor and Foldable instances to BinTree
-- since it's such a general and well known pattern, Haskell is able to figure
-- out how to implement these instances for us
-- I can now use the higher order functions fmap and foldr on BinTree
-- foldr (+) 0 exampleTree
-- = 29
-- fmap (1+) exampleTree
-- = Node (Node (Leaf 6) (Leaf 7) 8) (Leaf 4) 9