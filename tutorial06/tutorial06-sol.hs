import GHC.IO.Handle.Types (HandleType(ReadWriteHandle))
import GHC.IO.Buffer (emptyBuffer)
-- Table of Contents
-- 1. Recursive Algorithms

-- 2. Sorting Algorithms
-- 2.1 BubbleSort
-- 2.2 Insertion Sort
-- 2.3 QuickSort

-- 3. Input/Output
-- 3.1 Pure vs Impure Functions
-- 3.2 The Print Function
-- 3.4 The Do Notation
-- 3.5 Other Output Functions
-- 3.6 Output
-- 3.7 Input Functions
-- 3.8 Caveats
-- 3.9 Other Input Functions
-- 3.10 Don't Mix IO and Pure Functions

--------------------------------------------------------------------------------

-- 1. Recursive Algorithms

-- The basic pattern...
-- Base Case (s): The simplest case(s) that can be solved directly.
-- rec_function [] = ...
-- Recursive Case (s): The problem is reduced to a simpler/smaller version of
-- itself.
-- rec_function (x:xs) = ... rec_function xs ...

-- We'll implement 3 functions, zip, drop, and filter, using recursion.

-- zip: Given two lists, return a list of pairs where each pair contains the
-- corresponding elements of the input lists.
zip' :: [a] -> [b] -> [(a,b)]
zip' [] _ = []
zip' _ [] = []
zip' (x:xs) (y:ys) = (x,y) : zip' xs ys
-- zip' [1,2] ['a','b']
-- (1,'a') : zip' [2] ['b']
-- (1,'a') : (2,'b') : zip' [] []
-- (1,'a') : (2,'b') : []
-- [(1,'a'),(2,'b')]

-- drop: Given a list and a number n, return the list with the first n elements
-- removed.

drop' :: Int -> [a] -> [a]
drop' _ [] = []
drop' n (x:xs)
  | n <= 0 = x:xs
  | otherwise = drop' (n-1) xs
-- drop 2 [1,2,3,4]
-- drop 1 [2,3,4]
-- drop 0 [3,4]
-- [3,4]

-- filter: Given a predicate and a list, return a list of elements that satisfy
-- the predicate.
filter' :: (a -> Bool) -> [a] -> [a]
filter' _ [] = []
filter' p (x:xs)
  | p x = x : filter' p xs
  | otherwise = filter' p xs
-- filter even [1,2,3,4]
-- filter even [2,3,4]
-- 2 : filter even [3,4]
-- 2 : filter even [4]
-- 2 : 4 : filter even []
-- 2 : 4 : []
-- [2,4]

--------------------------------------------------------------------------------

-- 2. Sorting Algorithms

-- 2.1 BubbleSort
-- The basic idea is to repeatedly swap adjacent elements if they are in the
-- wrong order.
-- The algorithm gets its name from the way smaller elements "bubble" to the
-- top of the list.
-- An extremely inefficient algorithm, but it's simple to implement.

-- Let's do an example with 4 numbers: 4, 2, 1, 3
-- 4 2 1 3
-- 2 4 1 3
-- 2 1 4 3
-- 2 1 3 4
-- 1 2 3 4

-- a helper function that swaps two elements in a list
bubble :: Ord a => [a] -> [a]
bubble (x0:x1:xs)
  | x0 > x1 = x1 : bubble (x0:xs)
  | otherwise = x0 : bubble (x1:xs)
bubble xs = xs

-- implements the bubble sort algorithm
-- the ys == xs check to see if we made any swaps in the last iteration
-- if we didn't, the list is sorted and we return it
-- otherwise, we call bubbleSort recursively
bubbleSort xs =
  let ys = bubble xs
  in if ys == xs then xs else bubbleSort ys

-- 2.2 Insertion Sort
-- The basic idea is to build up a sorted list by inserting elements from the
-- input list one at a time.

-- Let's do an example with 4 numbers: 4, 2, 1, 3
-- []
-- [4]
-- [2,4]
-- [1,2,4]
-- [1,2,3,4]

insert :: Ord t => t -> [t] -> [t]
insert x [] = [x]
insert x (y:ys)
  | x <= y = x:y:ys
  | otherwise = y : insert x ys

insertionSort :: Ord a => [a] -> [a]
insertionSort [] = []
insertionSort (x:xs) = insert x (insertionSort xs)

-- insertionSort [4,2,5]
-- insert 4 (insertionSort [2,5])
-- insert 4 (insert 2 (insertionSort [5]))
-- insert 4 (insert 2 (insert 5 (insertionSort [])))
-- insert 4 (insert 2 (insert 5 []))
-- insert 4 (insert 2 [5])
-- insert 4 [2,5]
-- [2,4,5]

-- 2.3 QuickSort

-- choose a pivot element from the list
-- partition the list into two sublists: elements less than the pivot and
-- elements greater than the pivot
-- recursively sort the sublists
-- combine the sorted sublists with the pivot in between

-- Let's do an example with 4 numbers: 3,2,4,1,5
-- show the image provided in the slides

quickSort :: Ord a => [a] -> [a]
quickSort [] = []
quickSort (p:xs) =
  let less = filter (< p) xs
      more = filter (>= p) xs
  in quickSort less ++ [p] ++ quickSort more

-- quickSort [4,2,1,3]
-- quickSort [2,1,3] ++ [4] ++ quickSort []
-- (quickSort [1] ++ [2] ++ quickSort [3]) ++ [4] ++ []
-- ((quickSort [] ++ [1] ++ quickSort []) ++ [2] ++ quickSort [3]) ++ [4] ++ []
-- (([] ++ [1] ++ []) ++ [2] ++ (quickSort [] ++ [3] ++ quickSort [])) ++ [4] ++ []
-- (([] ++ [1] ++ []) ++ [2] ++ ([] ++ [3] ++ [])) ++ [4] ++ []
-- (([] ++ [1] ++ []) ++ [2] ++ [3]) ++ [4] ++ []
-- ([] ++ [1] ++ [2,3]) ++ [4] ++ []
-- [1,2,3] ++ [4] ++ []
-- [1,2,3,4]

--------------------------------------------------------------------------------

-- 3. Input/Output

-- 3.1 Pure vs Impure Functions
-- Pure functions: Given the same input, always return the same output.
-- Impure functions: Can return different outputs for the same input.
-- Impure functions can have side effects, such as reading from a file or
-- writing to the console.

-- An example in Python of a pure function
-- def add(x, y):
--   return x + y

-- An example in Python of an impure function
-- int counter = 0
-- def increment():
--   global counter
--   counter += 1
--   return counter

-- Haskell functions are "lazy" by default, meaning they don't evaluate their
-- arguments until they are needed.

uselessArithmetic :: Num p1 => p1 -> p2 -> p1
uselessArithmetic x y = let
    -- order of square1, square2, square3 doesn't matter
    -- even if we specify square3 first, it won't be evaluated until it's needed
    -- square1 will be evaluated first, then square2, then square3
    square3 = square2 * x
    square1 = x
    square2 = square1 * x
  in square3

uselessArithmetic2 = let
    x = 1
    y = sum [1..] -- infinite list, never evaluated because it's not used
  in uselessArithmetic x x

-- 3.2 The Print Function

-- print "Hello, World!"
-- this will print "Hello, World!" to the console
-- different than GHCi regurgitating the result of an expression, which is
-- the default behavior; this way, we can manually control what gets printed

printHello :: IO () -- the () type is called "unit" and is similar to void in C
printHello = print "Hello, World!"

-- similar to us writing
stringHello :: String
stringHello = "Hello, World!"
-- and then calling GHCi to print stringHello
-- Prelude> stringHello
-- "Hello, World!"

-- how is this different? GHCi will print the result of the expression, but
-- we can't control what gets printed; GHCi is used for testing and debugging
-- but if we want to build a program, we want to control what gets printed

-- The Haskell Compiler will use the main function as the entry point for the
-- program; it will execute the IO action returned by main

main :: IO ()
main = print "Hello, World!"

-- print' :: Show a => a -> IO ()
-- IO is a special data constructor
-- the () is an empty type, similar to void in C
-- print returns an IO value

-- 3.4 The Do Notation

-- Haskell functions are evaluated in a lazy manner, meaning they are only
-- evaluated when needed

-- IO needs to be sequenced in a specific order

-- we can use the do notation to sequence IO actions
printNonsense :: IO ()
printNonsense = do
  print "Hello, World!"
  print "Goodbye, World!"

-- 3.5 Other Output Functions

-- putStr: Similar to print, but doesn't add a newline character
-- putStr' :: String -> IO ()

-- writes a string to a file, creates or overwrites the file
-- writeFile' :: FilePath -> String -> IO ()

-- appends a string to a file, which must already exist
-- appendFile' :: FilePath -> String -> IO ()

-- 3.6 Output

-- when printed, it's printed with quotes
printSomeStuff = do
  print "Some"
  print "Stuff"

-- outputs without quotes, because print adds quotes
printSomeStuff' = do
  putStrLn "Some"
  putStrLn "Stuff"

printSomeStuff'' = do
  putStr "Some"
  putStrLn "Stuff"

-- creates file at the directory you're running the program from
makeAFile = do writeFile "test.txt" "Hello, World!"
               writeFile "test.txt" "Goodbye, World!"

-- can also specify file path, if the directory exists
makeAFile' = do writeFile "test/test.txt" "Hello, World!"

-- appends to the file
makeAFile'' = do writeFile "test.txt" "Hello, World!\n"
                 appendFile "test.txt" "Goodbye, World!"

-- 3.7 Input Functions

-- reads a line of text from the console
-- getLine :: IO String

-- reads a character from the console
-- getChar :: IO Char

-- reads a file and returns its contents as a string
-- readFile :: FilePath -> IO String

-- echo going to take user input, prints it out to the console
echo = do
  line <- getLine
  putStrLn line

-- you can't do line = getLine, because getLine is an IO action
-- so, we use the special <- operator to bind the result of the IO action to a
-- variable
-- this is special syntax for working with IO, basically saying, "get rid of the
-- IO wrapper and give me the value"

echoFile = do
  contents <- readFile "test.txt"
  putStrLn contents

-- 3.8 Caveats

-- Every line of do structure must return IO a
-- the <- operator takes a function of type IO a and returns a value of type a
-- last line cannot use <-, because it must return IO a
-- any function calling an IO function must also be an IO function


-- We can never go from IO a -> a...
-- ...but we can go from a -> IO a

-- pull :: IO a -> a
-- pull x = x

-- why is this the case?
-- Haskell is trying to prevent side effects
-- if we could go from IO a -> a, we could pull out the value from an IO action
-- and use it in a pure function, which would defeat the purpose of IO

-- Pure functions can't call IO functions, IO functions can call pure functions
-- this ensures that side effects are contained within IO functions ONLY

-- This makes debugging easier, because we know that if a function is pure, it
-- can't have side effects

-- 3.9 Other Input Functions

-- We can turn a into IO a, but not the other way around
-- return :: a -> IO a

-- We can use this to convert pure values in an IO

get2Lines :: IO String
get2Lines = do
  line1 <- getLine
  line2 <- getLine
  return (line1 ++ line2)

-- We can also do this recursively

echoForever :: IO ()
echoForever = do
  line <- getLine
  print line
  echoForever

-- we can also make this function stop when a certain condition is met
-- we can use the return function to return an IO action that does nothing
echoUntilQuit :: IO ()
echoUntilQuit = do
  line <- getLine
  if line == "quit"
    then return ()
    else do
      print line
      echoUntilQuit

-- The show function converts a value into a string
-- show :: Show a => a -> String

-- Good for outputting results with print
add :: Num a => a -> a -> a
add x y = x + y

main' :: IO ()
main' = do print ("5 + 4 = " ++ show (add 5 4))

-- The read function converts String to value
-- read :: Read a => String -> a

-- you usually need to specify the type of the value you're reading, but Haskell
-- can also infer the type
addInts :: IO Int
addInts = do
  x <- getLine
  y <- getLine
  return (read x + read y)

addStrings :: String -> String -> Int
addStrings x y = 
  let
    x' = read x :: Int -- here we convert the string to an Int
    xs = map read [x, y] :: [Int] -- here we convert the strings to a list of Ints
  in sum xs + 0*x'

-- Lines/Unlines Functions

-- lines: splits a string into a list of strings at newline characters
-- lines :: String -> [String]

-- a usecase: reading a file and splitting it into lines, since readFile only
-- returns a single string and we may want to work with individual lines
-- readFile "test.txt" >>= return . lines

-- unlines: joins a list of strings into a single string with newline characters
-- unlines :: [String] -> String

-- a usecase: writing a list of strings to a file, since writeFile only takes a
-- single string
-- writeFile "test.txt" . unlines $ ["Hello", "World"]

-- 3.10 Don't Mix IO and Pure Functions

-- here we are mixing IO and pure functions
-- this is bad practice, because it makes debugging harder
badCode :: IO ()
badCode = do
  x <- getLine
  x' <- return (read x :: Int)
  y <- getLine
  y' <- return (read y :: Int)
  z <- return (x' + y')
  z' <- return z
  print z'

-- instead, we should keep IO and pure functions separate
-- this makes it easier to debug, because we know that if a function is pure, it
-- can't have side effects
addStrings' :: String -> String -> Int
addStrings' x y = 
  let
    x' = read x :: Int
    y' = read y :: Int
  in x' + y'

betterCode :: IO ()
betterCode = do
  x <- getLine
  y <- getLine
  print (addStrings' x y)