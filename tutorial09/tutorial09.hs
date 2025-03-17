-- Table of Contents
-- 1. Introduction to Higher Order Functions
-- 2. Functions as Arguments
-- 3. Partial Application
-- 4. Currying and Uncurrying
-- 5. Function Combinators
-- 6. Constructors as Functions

--------------------------------------------------------------------------------
-- 1. Introduction to Higher Order Functions
--------------------------------------------------------------------------------

-- Higher Order Functions
-- functions are data in Haskell, can be treated just like data of any other type
-- this is known as first class functions, allows for some useful properties

-- Properties of HOFs
-- 1. Functions can be passed as arguments to other functions
-- 2. Functions can be defined using partial application
-- 3. Functions can be combined using operators, just like numbers can be
-- combined using +, -, *, and /

--------------------------------------------------------------------------------
-- 2. Functions as Arguments
--------------------------------------------------------------------------------

-- Many functions take in other functions as arguments: map, filter, foldr
-- These functions are known as higher order functions

-- Example: zipWith takes a function as its first argument
-- the function takes in two arguments, one from each list, and returns a value
-- the second and third arguments are lists, and zipWith returns a list of the
-- results of applying the function to each pair of elements
zipWith' :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith' _ [] _ = []
zipWith' _ _ [] = []
zipWith' f (x:xs) (y:ys) = f x y : zipWith' f xs ys

-- Functional programming makes use of chaining Higher Order Functions

-- Haskell provides many mechanisms to do so
even' :: Int -> Bool
even' x = x `mod` 2 == 0

succ' :: Int -> Int
succ' x = x + 1

-- f [1,2,3,4]
-- [2,3,4,5]
-- [2,4]


f :: Integral a => [a] -> [a]
f xs = filter even (map succ xs)
-- f xs = filter even $ map succ xs
-- f = \xs -> filter even $ map succ xs
-- f = filter even . map succ
-- f = filter (\x -> x `mod` 2 == 0) . map (+1)

--------------------------------------------------------------------------------
-- 3. Partial Application
--------------------------------------------------------------------------------

-- consider the following 2-ary function (a function taking two arguments)
add :: Int -> Int -> Int
add x y = x + y

-- we can define a unary function using partial application
incr :: Int -> Int
incr = add 1 -- the argument is implicit, it is 1

-- because of currying, any function with more than one argument can return
-- a function

-- for instance, the flip function, which takes a function and returns a
-- function that takes the arguments in reverse order
flip' :: (a -> b -> c) -> b -> a -> c
flip' f = \y x -> f x y

divFlipped :: Double -> Double -> Double
divFlipped = flip' (/)

-- The Syntax of application and ->
-- function application is left associative
-- f x y == (f x) y -- this is how Haskell interprets it
-- f x y /= f (x y) -- NOT the same

-- The function symbol -> is right associative
-- a -> b -> c == a -> (b -> c) -- this is how Haskell interprets it
-- a -> b -> c /= (a -> b) -> c -- NOT the same

--------------------------------------------------------------------------------
-- 4. Currying and Uncurrying
--------------------------------------------------------------------------------

-- by default, all functions are curried and allow for partial application
add'' :: Int -> Int -> Int
add'' x y = x + y

-- you can create an uncurried function by using a tuple
addUncurried :: (Int, Int) -> Int
addUncurried (x, y) = x + y

--------------------------------------------------------------------------------
-- 5. Function Combinators
--------------------------------------------------------------------------------

-- functions can be combined using other functions (combinators)

-- function composition, same as in math...
-- infixr 9 .
-- (.): (b -> c) -> (a -> b) -> a -> c
-- f . g = \x -> f (g x)
-- take the a value and apply g to it, then apply f to the result

-- before, we defined a function f as follows:
f' :: Integral a => [a] -> [a]
f' xs = filter even (map (+1) xs)
-- this function goes over a list, adds 1 to each element, and filters out
-- the even elements

-- now, we can use point free style to define f
-- we don't need to mention the argument xs, it is implicit
f'' :: Integral a => [a] -> [a]
f'' = (filter even) . (map (+1))

-- the $ combinator is an alternative to parentheses, takes advantage of the
-- associativity of function application, and is right associative

-- ($): (a -> b) -> a -> b
-- f $ x = f x

-- example
jimmy xs ys = foldr max 0 (filter even (map (+1) (concat (xs:[ys]))))

jimmy2 xs ys = foldr max 0 $ filter even $ map (+1) $ concat $ xs:[ys]

-- another alternative is the pipe, which is not defined in the Prelude
(|>) :: a -> (a -> b) -> b
x |> f = f x
-- this is the reverse of the $ combinator, it is left associative


-- it takes a value and applies a function to it on the right
-- helpful for reordering functions in a more readable way

-- version 1
-- putStrLn $
-- map toLower $
-- concat $
-- intersperse " " $
-- ["Hello", "Goodbye"]

-- version 2
-- ["Hello", "Goodbye"] |>
-- intersperse " " |>
-- concat |>
-- map toLower |>
-- putStrLn

--------------------------------------------------------------------------------
-- 6. Constructors as Functions
--------------------------------------------------------------------------------

-- Constructors are also functions
data Student = StudentC { name :: String, ident :: Int }

buildData :: [String] -> [Int] -> [Student]
buildData xs ys = zipWith StudentC xs ys