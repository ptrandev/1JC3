--
-- NOTE: For this tutorial, I'll be reimplementing some functions that are already in the Prelude
-- This is just for practice and to help you understand how these functions work!
-- In real code, you should always use the functions in the Prelude
-- For example, the Prelude has a function called "length" that does the same thing as "length'"
-- I'm just reimplementing it here for practice
-- Sometimes I call the version of the function I made or the built in version interchangably
-- This is denoted with a ', for example, "length" and "length'"
-- Sometimes this is necessary to ensure the code compiles, but it's also good practice to understand how these functions work
--

-- Today's Topics
-- 1. Curried Functions
-- 2. Uncurried Functions
-- 3. Polymorphic Functions
-- 4. Function Composition
-- 5. Type Classes
-- 6. Overloaded Functions
-- 7. Lambda Expressions
-- 8. Recursion (Map)
-- 9. List Patterns (Refresher)

--
-- 1. Curried Functions
--

add :: Int -> Int -> Int
-- is an equivalent type signature to
-- add :: Int -> (Int -> Int)
add x y = x + y

-- We can apply arguments to "add" one at a time
-- and get back a function that takes the next argument
add3 :: Int -> Int
add3 = add 3

-- Argument y is implicitly passed to the function returned
-- a more verbose way to write this would be...
add3' :: Int -> Int
add3' y = add 3 y

-- Functions are curried by default in Haskell
-- useful for working with higher order functions (functions that take functions as arguments and returns a function as a result), such as map
result = map (add 3) [1, 2, 3]

--
-- 2. Uncurried Functions
--

-- use a tuple to pass multiple arguments
-- now, the function takes a single argument, which is a tuple of two integers
add' :: (Int, Int) -> Int
add' (x, y) = x + y

--
-- 3. Polymorphic Functions
--

-- A function that takes in a list of any type and returns the length of the list
-- "a" is not a concrete type, but a type variable
-- how to know? Type variables are lowercase and concrete types are uppercase
-- we can use anything in place of "a", but it's convention to use single letters
length' :: [a] -> Int
length' [] = 0
length' (_:xs) = 1 + length' xs

-- A function that in a tuple and returns the first element
-- With a type signature that uses type variables, we can use the function with any type of tuple
-- We won't have to write a separate function for each type of tuple!
fst' :: (a, b) -> a
fst' (x, _) = x

-- so we don't have to do this:
fstInt :: (Int, b) -> Int
fstInt (x, _) = x

fstString :: (String, b) -> String
fstString (x, _) = x

-- A function that takes in a list of any type and returns the list in reverse order
-- NOTE: remember, although "a" can be ANY type, it MUST be the same type throughout the list
-- so we can't reverse a list of mixed types
reverse' :: [a] -> [a]
reverse' [] = []
reverse' (x:xs) = reverse' xs ++ [x]

-- for instance the following isn't allowed, since we don't perform any
-- operations to change a into some other type
-- reverse'' :: [a] -> [b]
-- reverse'' [] = []
-- reverse'' (x:xs) = reverse'' xs ++ [x]

-- neither is the following
-- result = reverse'' [1, "hello", 3]

-- A function that takes in a list of any type and returns the first element
head' :: [a] -> a
head' [] = error "Empty list"
head' (x:_) = x

-- A function that takes in a list of any type and returns the tail of the list
-- takes a function as an argument
-- turns a list of a and returns a list of b
map' :: (a -> b) -> [a] -> [b]
map' _ [] = []
map' f (x:xs) = f x : map' f xs

-- map' fst' [(1, True), (2, False), (3, True)]
-- returns [1, 2, 3]

-- How this makes sense given the type signature...
-- Think of it as substitution in a math equation!
-- map' :: (a -> b) -> [a] -> [b]
-- fst' :: (x, y) -> x
-- (a -> b) = (x, y) -> x
-- a = (x, y)
-- b = x
-- [a] = [(x, y)]
-- [b] = [x]

--
-- 4.Function Composition
--

-- if a function g has output of the same type as the input of function f
-- we can compose the two functions in the function z
-- z = f . g
lastElem :: [a] -> a
lastElem = head' . reverse'

-- notice, we have no argument! In this case, the argument is implicit
-- a more verbose way to write this would be...
lastElem' :: [a] -> a
lastElem' xs = head' (reverse' xs)

-- Parenthesis... using $ to avoid parenthesis
-- for example, the following two lines are equivalent
f (x:xs) = foldr ((+) . (+2)) 0 (tail (reverse (x:xs)))
f' (x:xs) = foldr ((+) . (+2)) 0 (reverse $ init x:xs)

--
-- 5. Type Classes
--

-- Define functions with different implementations depending on the type of their input (methods)
-- the EQ class contains methods!
class Eq' a where
  (==) :: a -> a -> Bool
  (/=) :: a -> a -> Bool

-- make an instance to define the methods for a specific type
instance Eq' Bool where
  True == True = True
  False == False = True
  _ == _ = False

  x /= y = not (x Prelude.== y)

instance Eq' Int where
  x == y = x Prelude.== y
  x /= y = not (x Prelude.== y)


-- Let's do it for a completely made up class called Example

class Example a where
  ex :: a -> a

instance Example Int where
  ex x = x + 1

instance Example Bool where
  ex x = not x

-- Now we can use the ex function with Ints and Bools
-- ex 3
-- ex True


--
-- 6. Predefined Classes
--

-- These are classes that are predefined in the Prelude
-- as we've seen, Eq is a class that defines how == and /= work for different
-- types

-- Show the Image in the folder...
-- What does the => symbol mean?
-- It's a class constraint, it means "is a member of"
-- so Eq => Ord means that for Ord to be valid, the type must also be a member of Eq
-- Ord is a subclass of Eq
-- Ord is used for types that have an ordering

-- Overloaded Functions
-- Polymorphic functions are overloaded if the type contains one or more class constraints
-- sum works for any type.. as long as the type a is a member of the Num class
-- the => symbol is a class constraint, it means "is a member of"
-- the following function is overloaded
sum' :: Num a => [a] -> a
sum' [] = 0
sum' (x:xs) = x + sum' xs

-- Remember our add function? That only works for Ints
-- What if we want it to work for any type that is a member of the Num class?
add'' :: Num a => a -> a -> a
add'' x y = x + y

-- We could do the same for division...
divide' :: Float -> Float -> Float
divide' x y = x / y

-- But this is not the most general type possible if we want it to work for any type that is a member of the Fractional class
divide'' :: Fractional a => a -> a -> a
divide'' x y = x / y

-- In GHCI...
-- we can use :info to see the type of a function
-- :info divide

-- it will show you the instances of the class as well!
-- :info Fractional

-- We can also decide which instance of a class to use
-- div 5.0 2.5 :: Float
-- div 5.0 2.5 :: Double

-- Why do we want to do this? To avoid rounding errors! Float is less precise than Double

-- Same with Int and Integer, Integer is as big as your memory can handle
-- Int is a fixed size, usually 32 or 64 bits

--
-- 7. Lambda Expressions
--

-- functions without a name are called lambda expressions
-- \x -> x + x
-- is the same as
-- double x = x + x
-- type signature is the same as well
-- double :: Int -> Int

-- good for defining functions that return a function as a result
-- add :: Int -> Int -> Int
-- add x y = x + y
-- is the same as
-- add = \x -> (\y -> x + y)
-- explanation: add takes in x and returns a function that takes in y and returns x + y
-- done using currying


-- where is this useful? map!
-- map takes a function as an argument and a list, applies the function to each element of the list, and returns a new list
add1 :: Int -> Int
add1 x = x + 1

sum1 :: [Int] -> [Int]
sum1 xs = map add1 xs

-- sum1 [1,2,3] returns [2,3,4]

-- lambda expressions are useful for defining functions that are only used once
-- sum1' is the same as sum1
sum1' xs = map (\x -> x + 1) xs

--
-- 8. Recursion
--
map'' :: (a -> b) -> [a] -> [b]
map'' _ [] = []
map'' f (x:xs) = f x : map'' f xs

-- let's run through how sum1 would work with map
-- sum1 [1,2,3]
-- map add1 [1,2,3]
-- [add1 1, add1 2, add1 3]
-- [1 + 1, 2 + 1, 3 + 1]
-- [2, 3, 4]
-- look at the screenshot in the folder for a visual representation

--
-- 7. List Patterns
--

-- [1,2,3,4,5] is the same as 1:(2:(3:(4:(5:[]))))
-- this is called cons notation
-- elements until the the empty list are separated by colons

-- x:xs doesn't pattern match on the empty list, so remember to handle that case!
-- Functions can be defined using (x:xs) patterns
-- x is the head of the list, xs is the tail of the list
head :: [a] -> a
head [] = error "Empty list" -- no way to handle this case due to the type signature
head (x:xs) = x

tail' :: [a] -> [a]
tail' [] = []
tail' (x:xs) = xs