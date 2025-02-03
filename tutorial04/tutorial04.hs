-- Topic List
-- 1. Recursion
-- 2. Recursive Types
-- 3. Arithmetic Expressions
-- 4. Binary Trees
-- 5. Binary Search Trees

-- 1. Recursion

-- Recursive Function
-- A function that calls itself...

-- Rules:
-- 1. Contain a base case...
-- 2. Contain a recursive case..
-- 3. Make progress towards the base case...

-- m^n
-- 2^2 = 2 * 2 = 4
-- 2^4 = 2 * 2 * 2 = 8
pow _ 0 = 1 -- base case
pow m n = m * pow m (n-1) -- recursive case

-- 2. Recursive Types
-- Recursion to define types w/ an infinite amount of values

-- Natural Numbers
-- 0, 1, 2, 3, 4, ...
-- specifically ensure that a number is always positive
data Nat = Zero | Succ Nat
  deriving Show -- tells Haskell to automatically generate a show function for Nat so we can print it in GHCI

-- Nat is a with two constructors
-- Zero :: Nat
-- Succ :: Nat -> Nat

-- Nat contains an infinite amount of values...
-- Base case... 0: Zero
-- Recurse case...
-- 1: Succ Zero
-- 2: Succ (Succ Zero)
-- 3: Succ (Succ (Succ Zero))

zero = Zero
one = Succ Zero
two = Succ (one) -- Succ (Succ Zero)
three = Succ (two) -- (Succ (Succ (Succ Zero)))

nat2int :: Nat -> Int
-- base case
nat2int Zero = 0
-- recursive case
nat2int (Succ n) = 1 + nat2int n

-- nat2int (Succ (Succ (Succ Zero)))
-- = 1 + nat2int (Succ (Succ Zero))
-- = 1 + 1 + nat2int (Succ Zero)
-- = 1 + 1 + 1 + nat2int Zero
-- = 1 + 1 + 1 + 0
-- = 3

int2Nat :: Int -> Nat
-- base case
int2Nat 0 = Zero
-- recursive case
int2Nat n = Succ (int2Nat (n-1))

-- int2nat 3
-- = Succ (int2Nat(3-1))
-- = Succ (in2Nat(2))
-- = Succ (Succ (int2Nat(2-1)))
-- = Succ (Succ (int2Nat(1)))
-- = Succ (Succ (Succ (int2Nat(1-1))))
-- = Succ (Succ (Succ (int2Nat(0))))
-- = Succ (Succ (Succ Zero)))

add:: Nat -> Nat -> Nat
-- add m n
-- base case
add Zero n = n
-- recursive case
add (Succ m) n = Succ (add m n)

{-
  add (Succ(Succ Zero)) (Succ Zero)
  = Succ (add (Succ Zero) (Succ Zero))
  = Succ (Succ (add (Zero) (Succ Zero)))
  = Succ (Succ (Succ Zero))
-}

-- 3. Arithmetic Expressions

-- 1 + 2 * 3
--  +
-- / \
--1  *
--  / \
-- 2   3

-- 1 + 2 * 3
-- 1 + 6
-- 7

data Expr = Val Int | Add Expr Expr | Mul Expr Expr
  deriving Show

-- Add (Val 1) (Mul (Val 2) (Val 3))

eval :: Expr -> Int
-- base case
eval (Val n) = n
-- recursive cases
eval (Add x y) = eval x + eval y
eval (Mul x y) = eval x * eval y

-- eval (Add (Val 1) (Mul (Val 2) (Val 3)))
-- eval (Val 1) + (eval (Mul (Val 2) (Val 3)))
-- eval (Val 1) + (eval (Val 2) * eval (Val 3))
-- eval (Val 1) + (2 * 3)
-- eval (Val 1) + 6
-- 1 + 6
-- 7

-- "2 + 5 * 6"
-- = 32

-- 4. Binary Trees

--    5
--   / \
--  4   3
--     / \
--    2   1

-- we can represent this tree as a recursive data type
-- a tree is either a leaf or a node with two subtrees
data Tree = Leaf Int | Node Int Tree Tree
  deriving Show

tree :: Tree
tree = Node 5 (Leaf 4)
              (Node 3 (Leaf 2)
                      (Leaf 1))

occurs :: Int -> Tree -> Bool
-- base case
occurs m (Leaf n) = m == n
-- recursive case
occurs m (Node n l r) = m == n || occurs m l || occurs m r

-- OR (||) If ANY value is True, it will return True

-- use the ++ operator to put lists together
flatten :: Tree -> [Int]
-- BASE CASE
flatten (Leaf n) = [n]
-- RECURSIVE CASE
flatten (Node n l r) = flatten l ++ [n] ++ flatten r

-- example:
-- flatten tree
-- = flatten (Node 5 (Leaf 4) (Node 3 (Leaf 2) (Leaf 1)))
-- = flatten (Leaf 4) ++ [5] ++ flatten (Node 3 (Leaf 2) (Leaf 1))
-- = -- = [4] ++ [5] ++ flatten (Leaf 2) ++ [3] ++ flatten (Leaf 1)
-- = [4] ++ [5] ++ [2] ++ [3] ++ [1]
-- = [4, 5, 2, 3, 1]

-- 5. Binary Search Trees
-- a binary search tree is a binary tree where the left subtree contains values
-- less than the root and the right subtree contains values greater than the root

-- why does this matter? It means we can optimize our search algorithm for occurs
-- instead of looking at every single node, we can use the structure of the tree to
-- determine which subtree to search
-- this reduces the time complexity from O(n) to O(log n) in the worst case
-- where n is the number of nodes in the tree
-- log n is the height of the tree, which is the maximum number of steps to reach a leaf

--     4
--    / \
--   3   5
--  / \
-- 1   2

occursBST :: Int -> Tree -> Bool
-- base case
occursBST m (Leaf n) = m == n
-- recursive case
occursBST m (Node n l r)
  | m == n = True
  | m < n = occursBST m l
  | otherwise = occursBST m r