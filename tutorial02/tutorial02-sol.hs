-- Basic Conditional Statements
dbzDialog :: (Ord a, Num a) => a -> String
dbzDialog powerLevel = if powerLevel > 9000
                      then "It's over 9000!"
                      else "Meh, not that impressive."

-- Nested Conditional Statements
-- Use indentation to keep track of how many if expressions are nested.
-- Not necessary syntactically, but it makes the code more readable.
-- Example: Take integer n and return -1 if n is negative, 0 if n is zero, and 1 if n is positive.
ifSign :: Int -> Int
ifSign n = if n < 0
                then -1
                else (if n == 0
                    then 0
                    else 1)

-- Rules with if expressions...
-- 1. The else part is mandatory.
-- 2. The then and else parts must be indented the same amount.
-- 3. The then and else parts must be of the same type.
-- 4. The then and else parts must be present.

-- Guarded Equations
-- uses the | symbol to separate the condition from the expression
-- the otherwise keyword is used to catch all other cases
-- 1. Guarded equations are a more readable way to write conditional statements.
-- 2. They are more readable than nested if expressions.
-- Example:
guardedSign :: Int -> Int
guardedSign n
  | n < 0     = -1
  | n == 0    = 0
  | otherwise = 1

-- Pattern Matching
-- A way to match the structure of data with the structure of the pattern.
-- Example:
patternSign :: Int -> Int
patternSign 0 = 0
patternSign n | n < 0 = -1
patternSign _ = 1

-- Case Expressions
-- A way to pattern match in a more general context.
-- Example:
caseSign :: Int -> Int
caseSign n = case n of
  n | n < 0 -> -1
  n | n == 0 -> 0
  _ -> 1

-- Partial Functions: Undefined/Error
-- 1. Undefined: Covers cases that are not expected to be reached by the function.
-- The program should not crash if it reaches them.
-- Example:
undefinedFunction :: Int -> Int
undefinedFunction n = if n < 0 then undefined else n

-- 2. Error: Covers cases that are expected to be reached, but are not handled
-- by the function; the program should crash if it reaches them.
-- Example:
partialFunction :: Int -> Int
partialFunction n = if n < 0 then error "Negative number" else n

-- XOR Function: For getting you to think about how to solve Exercise 2
-- The XOR function is a function that returns true if exactly one of the arguments is true.

-- Example using pattern matching
xor :: Bool -> Bool -> Bool
xor True False = True
xor False True = True
xor _ _ = False

-- Example using guarded equations
xor' :: Bool -> Bool -> Bool
xor' x y
  | x && not y = True
  | not x && y = True
  | otherwise = False

-- Example using conditional expressions
xor'' :: Bool -> Bool -> Bool
xor'' x y = if x then not y
            else y

-- Join Function: For getting you to think about how to solve Exercise 2
-- The join function takes a list of strings and concatenates them together.
join :: [String] -> [Char]
join [] = ""
join [x] = x
join (x:xs) = x ++ join xs