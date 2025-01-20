-- Basic Conditional Statements
dbzDialog :: (Ord a, Num a) => a -> String
dbzDialog powerLevel = if powerLevel > 9000
                      then error "It's over 9000!"
                      else "Meh, not that impressive."

-- Nested Conditional Statements
numbersign :: Int -> Int
numbersign n = if n < 0 then -1 else if n == 0 then 0 else 1

-- Rules with if expressions...
-- 1. The else part is mandatory.
-- 2. The then and else parts must be indented the same amount.
-- 3. The then and else parts must be of the same type.
-- 4. The then and else parts must be present.

-- Partial Functions: Undefined/Error
-- 1. Undefined: Covers cases that are not expected to be reached by the function.
-- The program should not crash if it reaches them.
-- 2. Error: Covers cases that are expected to be reached, but are not handled
-- by the function; the program should crash if it reaches them.
-- Example:
partialFunction :: Int -> Int
partialFunction n = if n < 0 then error "Negative number" else n

-- Guarded Equations
-- uses the | symbol to separate the condition from the expression
-- the otherwise keyword is used to catch all other cases
-- 1. Guarded equations are a more readable way to write conditional statements.
-- 2. They are more readable than nested if expressions.
-- Example:
guardedsign :: Int -> Int
guardedsign n
  | n < 0     = -1
  | n == 0    = 0
  | otherwise = 1

-- Pattern Matching
-- A way to match the structure of data with the structure of the pattern.
-- Example:
patternMatch :: Int -> String
patternMatch 0 = "Zero"
patternMatch 1 = "One"
patternMatch 2 = "Two"
patternMatch _ = "Other"