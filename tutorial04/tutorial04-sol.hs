-- Topic List
-- 1. Recursion
-- 2. Recursive Types
-- 3. Arithmetic Expressions
-- 4. Binary Trees
-- 5. Binary Search Trees

-- 1. Recursion

-- Recursive Function
-- A recursive function is a function that calls itself.

-- Some rules to follow when writing recursive functions:
-- 1. A recursive function must have a base case.
-- 2. A recursive function must have a recursive case.
-- 3. A recursive function must make progress towards the base case.

-- m ^ n
-- an incorrect way.. no base case
-- pow m n = m * pow m (n - 1)
{-
  pow 2 2
    = 2 * pow 2 (2-1)
    = 2 * pow 2 1
    = 2 * (2 * pow 2 (1-1))
    = 2 * (2 * pow 2 0)
    = 2 * (2 * (2 * pow 2 (-1)))
    = 2 * (2 * (2 * (2 * pow 2 (-2))))
-}

-- m ^ n
-- a correct way
pow :: Int -> Int -> Int
pow _ 0 = 1
pow m n = m * pow m (n - 1)

{-
  pow 2 2
    = 2 * pow 2 (2-1)
    = 2 * pow 2 1
    = 2 * (2 * pow 2 (1-1))
    = 2 * (2 * 1)
    = 2 * 2
    = 4
-}

-- 2. Recursive Types
-- We can use recursion to define types with an infinite amount of values

-- Natural Numbers
-- 0, 1, 2, 3, 4, ...
-- specifically useful if you want to ensure that a number is always positive
data Nat = Zero | Succ Nat
  deriving Show -- tells Haskell to automatically generate a show function for Nat so we can print it in GHCI

-- Nat is a new type with two constructors
-- Zero :: Nat
-- Succ :: Nat -> Nat

-- Nat contains an infinite amount of values
-- 0: Zero (base case)
-- 1: Succ Zero
-- 2: Succ (Succ Zero)
-- ...

zero = Zero
one = Succ Zero
two = Succ (one)
three = Succ (two)

-- we can define a function that converts a Nat to an Int
-- this uses recursion to count the number of Succ constructors
nat2int :: Nat -> Int
nat2int Zero = 0
nat2int (Succ n) = 1 + nat2int n

-- we can also define a function that converts an Int to a Nat
-- this uses recursion to create a chain of Succ constructors
int2Nat :: Int -> Nat
int2Nat 0 = Zero
int2Nat n = Succ (int2Nat (n - 1))

-- int2nat 10
-- = Succ (Succ ... (Succ Zero) ...)
-- nat2int (int2nat 10)
-- = 10

add :: Nat -> Nat -> Nat
add Zero n = n
add (Succ m) n = Succ (add m n)

{-
  add (Succ (Succ Zero)) (Succ Zero)
  = add (Succ Zero) (Succ (Succ Zero))
  = add Zero (Succ (Succ (Succ Zero)))
  = (Succ (Succ (Succ Zero)))
-}

-- 3. Arithmetic Expressions

-- Consider how 1 + 2 * 3 is evaluated
-- 1 + 2 * 3
--  +
-- / \
-- 1 *
--  / \
-- 2  3

-- We can use recursion to represent a tree of arithmetic expressions
-- according to the previous example
data Expr = Val Int | Add Expr Expr | Mul Expr Expr
  deriving Show

-- the previous example would be:
-- Add (Val 1) (Mul (Val 2) (Val 3))

-- Imagine we are building a calculator application...
-- it's better to represent the expression as a tree

-- 2 * 5 + 6
-- if we stored this as a string and evaluated it from left to right
-- we would get 22
-- but the correct answer is 16

eval :: Expr -> Int
eval (Val n) = n -- our base case
eval (Add x y) = eval x + eval y -- recursive call, this turns the x and y into Int and adds them together
eval (Mul x y) = eval x * eval y -- recursive call, this turns the x and y into Int and multiplies them together

-- eval (Add (Val 2) (Mul (Val 5) (Val 6)))

-- 4. Binary Trees
-- useful to store data in a two-way branching structure, called a binary tree
-- this is a more generalized example of the arithmetic expression tree

--    5
--   / \
--  4   3
--     / \
--    2   1

-- we can represent this tree as a recursive data type
-- a tree is either a leaf or a node with two subtrees
-- a leaf contains a value (base case)
-- a node contains a value and two subtrees (recursive case)
data Tree = Leaf Int | Node Int Tree Tree
  deriving Show

-- represents the tree above
tree :: Tree
tree = Node 5 (Leaf 4)
              (Node 3 (Leaf 2)
                      (Leaf 1))

-- tells us if the integer exists anywhere in the tree
-- use the || operator to combine the results of the recursive calls
-- if True is found in any of the recursive calls, then the result is True
-- if the base case is reached and the integer is not found, then the result is False
occurs :: Int -> Tree -> Bool
occurs m (Leaf n) = m == n
occurs m (Node n l r) = m == n || occurs m l || occurs m r

-- example:
-- occurs 2 tree
-- = occurs 2 (Node 5 (Leaf 4) (Node 3 (Leaf 2) (Leaf 1)))
-- = occurs 2 (Leaf 4) || occurs 2 (Node 3 (Leaf 2) (Leaf 1))
-- = False || occurs 2 (Leaf 2) || occurs 2 (Leaf 1)
-- = False || True || False
-- = True

-- use the ++ operator to put lists together
flatten :: Tree -> [Int]
flatten (Leaf n) = [n]
flatten (Node n l r) = flatten l ++ [n] ++ flatten r

-- example:
-- flatten tree
-- = flatten (Node 5 (Leaf 4) (Node 3 (Leaf 2) (Leaf 1)))
-- = flatten (Leaf 4) ++ [5] ++ flatten (Node 3 (Leaf 2) (Leaf 1))
-- = [4] ++ [5] ++ flatten (Leaf 2) ++ [3] ++ flatten (Leaf 1)
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

-- example of a binary search tree
--     4
--    / \
--   3   5
--  / \
-- 1   2
-- if we were to flatten this tree, we would get [1, 2, 3, 4, 5]

occursBST :: Int -> Tree -> Bool
occursBST m (Leaf n) = m == n
occursBST m (Node n l r)
  | m == n = True
  | m < n = occursBST m l
  | otherwise = occursBST m r

-- example:
-- occursBST 2 (Node 4 (Node 3 (Leaf 1) (Leaf 2)) (Leaf 5))
-- = occursBST 2 (Node 3 (Leaf 1) (Leaf 2))
-- = occursBST 2 (Leaf 2)
-- = True