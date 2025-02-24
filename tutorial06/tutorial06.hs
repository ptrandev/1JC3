-- https://avenue.cllmcmaster.ca/d2l/le/content/683785/viewContent/5054078/View

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

-- Base Case
-- function [] =
-- Recursive Case
-- function (x:xs) =

-- zip: Given two lists, return a list of pairs where each pair contains the
-- corresponding elements of the input lists.

-- zip' [1,2] ['a','b']
-- [(1,'a'),(2,'b')]

zip' :: [a] -> [b] -> [(a,b)]
zip' [] _ = []
zip' _ [] = []
zip' (x:xs) (y:ys) = (x, y) : zip' xs ys

-- zip' [1,2] ['a','b']
-- (1, 'a') : zip [2] ['b']
-- (1, 'a') : (2, 'b') : zip [] []
-- (1, 'a') : (2, 'b') : []
-- [(1, 'a'), (2, 'b')]

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
-- [3, 4]

-- filter: Given a predicate and a list, return a list of elements that satisfy
-- the predicate.
filter' :: (a -> Bool) -> [a] -> [a]
filter' _ [] = []
filter' p (x:xs)
  | p x = x : filter' p xs
  | otherwise = filter' p xs
-- filter' even [1,2,3,4]
-- filter' even [2,3,4]
-- 2 : filter' even [3,4]
-- 2 : filter' even [4]
-- 2 : 4 : filter' even []
-- 2 : 4 : []
-- [2, 4]

--------------------------------------------------------------------------------

-- 2. Sorting Algorithms

-- 2.1 BubbleSort
-- The basic idea is to repeatedly swap adjacent elements if they are in the
-- wrong order.
-- The algorithm gets its name from the way larger elements "bubble" to the
-- top of the list.
-- An extremely inefficient algorithm, but it's simple to implement.


-- 4, 2, 1, 3
-- 2, 4, 1, 3
-- 2, 1, 4, 3
-- 2, 1, 3, 4
-- 1, 2, 3, 4

bubble :: Ord a => [a] -> [a]
bubble (x0:x1:xs)
  | x0 > x1 = x1 : bubble (x0:xs)
  | otherwise = x0 : bubble (x1:xs)
bubble xs = xs



bubbleSort :: Ord a => [a] -> [a]
bubbleSort xs =
  let ys = bubble xs
  in if ys == xs then xs else bubbleSort ys

-- bubbleSort[4,2,1,3]
  -- bubbleSort [2,1,3,4]
    -- bubbleSort [1,2,3,4]
      -- [1,2,3,4]

-- bubble [4,2,1,3]
-- 2 : bubble [4,1,3]
-- 2 : 1 : bubble [4,3]
-- 2 : 1 : 3 : bubble [4]
-- 2 : 1 : 3 : 4 : []
-- [2,1,3,4]

-- bubble [2,1,3,4]
-- 1 : bubble [2,3,4]
-- 1 : 2 : bubble [3, 4]
-- 1 : 2 : 3 : bubble [4]
-- 1 : 2 : 3 : 4
-- [1, 2, 3, 4]

-- 2.2 Insertion Sort
-- The basic idea is to build up a sorted list by inserting elements from the
-- input list one at a time.

-- Let's do an example with 4 numbers: 4, 2, 1, 3
-- []
-- [4]
-- [2, 4]
-- [1, 2, 4]
-- [1, 2, 3, 4]

insert :: Ord t => t -> [t] -> [t]
insert x [] = [x]
insert x (y:ys)
  | x <= y = x : y : ys
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
    square1 = x
    square3 = square2 * x
    square2 = square1 * x
  in square3

uselessArithmetic2 = let
    x = 1
    y = sum [1..] -- infinite list, never evaluated because it's not used
  in uselessArithmetic x x

-- 3.2 The Print Function
printHello :: IO () -- the () type is called "unit" and is similar to void in C
printHello = print "Hello, World!"

main :: IO ()
main = print "Hello, World!"

printNonsense :: IO ()
printNonsense = do
  print "Hello, World!"
  print "Goodbye, World!"

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

makeAFile = do writeFile "test.txt" "Hello, World!"
               writeFile "test.txt" "Goodbye, World!"

makeAFile'' = do writeFile "test.txt" "Hello, World!\n"
                 appendFile "test.txt" "Goodbye, World!"

-- 3.7 Input Functions

-- reads a line of text from the console
-- getLine :: IO String

-- reads a character from the console
-- getChar :: IO Char

-- reads a file and returns its contents as a string
-- readFile :: FilePath -> IO String

echo = do
  line <- getLine
  putStrLn line

echoFile = do
  contents <- readFile "test.txt"
  putStrLn contents

-- 3.8 Caveats

-- Every line of do structure must return IO a
-- the <- operator takes a function of type IO a and returns a value of type a
-- last line cannot use <-, because it must return IO a
-- any function calling an IO function must also be an IO function

-- pull :: IO a -> a
-- pull x = x

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

echoForever :: IO ()
echoForever = do
  line <- getLine
  print line
  echoForever

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

addInts :: IO Int
addInts = do
  x <- getLine
  y <- getLine
  return (read x + read y)

-- Lines/Unlines Function

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