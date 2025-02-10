-- Data Types

-- 1. Lists
-- Group together values via square brackets separated by commas
-- [1,5,3,2]

-- 2. Set Comprehension
-- In Math, comprehension notation allows you to construct new sets from old ones
-- {x^2 | x ∈ 5 {1,2,3,4,5}}
-- {1, 4, 9, 16, 25}

-- 3. List Comprehension
-- Similar, not for sets, but for lists
l = [x^2 | x <- [1..5]]
--- [1, 4, 9, 16, 25]

-- This exists in python too:
-- [x**2 for x in range(1,6)]
-- [1, 4, 9, 16, 25]

-- Expression x <- [1..5] is called a generator
-- Comprehensions can have multiple generators

list = [(x,y) | x <- [1,2,3], y <- [4,5]]
-- [(1,4), (1,5), (2,4), (2,5), (3,4), (3,5)]
list2 = [(x,y) | y <- [4,5], x <- [1,2,3]]
-- [(1,4), (2,4), (3,4), (1,5), (2,5), (3,5)]

-- as you can see, the order of the generators is important
-- This is because the first generator is the outermost loop
-- and the second generator is the innermost loop
-- This is similar to nested loops in python
-- for x in [1,2,3]:
--     for y in [4,5]:
--         print(x,y)

-- 4. Guards

-- Use guards in list comprehensions to restrict values produced by earlier generators
list3 = [x | x <- [1..10], even x]
-- [2,4,6,8,10]

-- use a guard to generate list of all factors of an integer
-- factors of 6 are 1,2,3,6
factors :: Int -> [Int]
factors n = [x | x <- [1..n], n `mod` x == 0]
-- use `mod` to get the remainder of integer division
-- can also use `div` to get the quotient, while / is for floating point division

-- 5. Type Declarations
-- New name for an existing type can be defined with a type declaration

-- type String = [Char]
-- for instance, a String is just a type synonym for a list of characters
-- this already exists in Haskell

-- inside of GHCI...

-- :type ['a','b','c']
-- ['a','b','c'] :: [Char]

-- ['a','b','c'] == "abc"
-- True

-- :type "abc"
-- "abc" :: [Char]

-- "abc" :: [Char]
-- "abc"

-- "abc" :: String
-- "abc"

-- 6. Type Synonyms
-- Can be used to make other types easier to read

-- now we can use this to more clearly define our code and understand it better
-- for instance, we if we're working with Coordinates on a screen, we can
-- define a type synonym for a pair of integers

type Pos = (Int, Int)

origin :: Pos
origin = (0,0)

left :: Pos -> Pos
left (x,y) = (x-1,y)

-- 7. Type Parameters

-- Type declarations and also have parameters
-- a is a type parameter, which means that the type Pair a is a pair of values of any type

type Pair a = (a, a)

-- here, I create a type signature specifically for a pair of integers
mult :: Pair Int -> Int
mult (m,n) = m*n

-- this one is specifically for a pair of floats
divide :: Pair Float -> Float
divide (m,n) = m/n

-- this one is polymorphic, meaning it can take any type
copy :: a -> Pair a
copy x = (x,x)

-- 8. Nested Types
-- type declarations can be nested

-- type Pos = (Int, Int)
-- This transposes a position on a screen
-- if you have a position (x,y), then the transposed position is (y,x)
type Transpose = Pos -> Pos

-- however, types cannot be used recursively
-- type Tree = (Int, [Tree])

-- 9. Data Declarations
-- a new type can be defined using data declaration
-- for instance, Bool is defined as follows in Prelude
-- data Bool = False | True

-- this means that we are creating completely new value, not just a synonym
-- We add in all the values we want our data declaration to take, separated by 
-- a vertical bar

-- Let's say we need a custom data type for answering a question, but we need
-- more than just True or False

-- These are values we're making, so they MUST begin with a capital letter
data Answer = Yes | No | Unknown

answers :: [Answer]
answers = [Yes, No, Unknown]

-- A function that flips the answer
flip :: Answer -> Answer
flip Yes = No
flip No = Yes
flip Unknown = Unknown


-- A more practical example, we can use data declarations to define a custom data type
-- with multiple constructors that represent different shapes
-- These constructors take arguments that represent the dimensions of the shape

data Shape = Circle Float | Rect Float Float

unitCircle :: Shape
unitCircle = Circle 1.0

unitSquare :: Shape
unitSquare = Rect 1.0 1.0

-- a function that allows us to construct a square
square :: Float -> Shape
square n = Rect n n

unitsquare2 = square 1.0

area :: Shape -> Float
area (Circle r) = pi * r^2
area (Rect x y) = x * y

-- Circle Float is actually a function on its own!
-- :type Circle 1.0
-- Circle 1.0 :: Shape
-- :type Circle
-- Circle :: Float -> Shape
-- :type Rect
-- Rect :: Float -> Float -> Shape

-- 10. Data Parameters
-- Data declarations themselves can also have parameters

-- In prelude, Maybe is defined as...
-- data Maybe a = Nothing | Just a

-- using this type, we can define safe division function
-- This allows us to divide without crashing the program
safeDiv :: Int -> Int -> Maybe Int
safeDiv _ 0 = Nothing
safeDiv m n = Just (div m n)

-- safeDiv 10 2
-- Just 5
-- safeDiv 10 0
-- Nothing

isJust :: Maybe a -> Bool
isJust (Just _) = True
isJust Nothing = False

fromJust :: Maybe a -> a
fromJust (Just x) = x
fromJust Nothing = error "fromJust: Nothing"

-- Example usage of safeDiv, isJust, and fromJust

-- Function to divide 10 by a list of numbers
divFrom10 :: [Int] -> [Maybe Int]
divFrom10 xs = [safeDiv 10 x | x <- xs]

-- Function to extract values from a list of Maybe Int, ignoring Nothings
extractValues :: [Maybe Int] -> [Int]
extractValues ms = [fromJust m | m <- ms, isJust m]

-- Example usage
example :: [Int]
example = extractValues (divFrom10 [1, 2, 0, 5])
-- example should be [10, 5, 2]

-- Remember how we used to define head of a list?
-- head :: [a] -> a
-- head [] = error "empty list"
-- head (x:_) = x

-- But with Maybe, we can avoid crashing the program!

safeHead :: [a] -> Maybe a
safeHead [] = Nothing
safeHead (x:_) = Just x

-- safeHead [1,2,3]
-- Just 1
-- safeHead []
-- Nothing

-- We can cerate a function to extract the value from a Maybe type, for using
-- in a program that expects a value, not a Maybe type