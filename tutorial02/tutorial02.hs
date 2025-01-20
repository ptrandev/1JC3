-- Tutorial 02

absVal x = if x >= 0 then x else -x

dbzDialog powerLevel = if powerLevel > 9000
                        then "It's over 9000!"
                        else "Meh, not impressive."

-- Takes integer n
-- If n is negative... return -1
-- If n is 0... return 0
-- If n is positive... return 1
ifSign :: Int -> Int
ifSign n = if n < 0
            then -1
            else
              if n == 0
                then 0
                else 1

-- Rules of if expressions...
-- 1. ELse is mandatory!
-- 2. The "then" part is also mandatory!
-- 3. The then and else parts must be of the same type!

-- Guarded Equation
-- Uses | symbol is used to separate the condition from the expression

-- Takes integer n
-- If n is negative... return -1
-- If n is 0... return 0
-- If n is positive... return 1
guardedSign n
  | n < 0     = -1
  | n == 0    = 0
  | otherwise = 1

-- Takes integer n
-- If n is negative... return -1
-- If n is 0... return 0
-- If n is positive... return 1
patternSign n | n < 0 = -1
patternSign 0 = 0
patternSign _ = 1

-- Case Expression
-- Takes integer n
-- If n is negative... return -1
-- If n is 0... return 0
-- If n is positive... return 1
caseSign n = case n of
  n | n < 0 -> -1
  n | n == 0 -> 0
  _ -> 1

-- Partial Functions: Undefined/Error
-- Undefined: Covers cases that are not expected to be reached by the function...
-- If it does reach them... it will crash

-- Error: Same thing, but takes in a string as an argument so you can set
-- your own error message!
undefinedFunction :: Int -> Int
undefinedFunction n = if n < 0 then n else error "error message!"

exampleFunction :: Int -> Bool
exampleFunction n
  | n == 0 = False
  | n == 1 = True
  | otherwise = error "Non-boolean integer!"

-- XOR Function:
-- XOR is a function that returns true if EXACTLY ONE of the arguments is True...
-- Otherwise, it will return false
xor :: Bool -> Bool -> Bool
xor True False = True
xor False True = True
xor _ _ = False

xor' :: Bool -> Bool -> Bool
xor' x y
  | x && not y = True
  | not x && y = True
  | otherwise  = False

xor'' x y = if x then not y else y

xor''' x y = case (x, y) of
  (True, False) -> True
  (False, True) -> True
  _ -> False

-- Finds any spaces in the string and removes them
-- I Love 1JC3!
-- ILove1JC3!
removeSpaces :: String -> String
removeSpaces "" = ""
removeSpaces (x:xs) = if x == ' ' then removeSpaces xs else x : removeSpaces xs

stack :: String -> String
stack "" = ""
stack (x:xs) = xs ++ [x]

-- Thank you!! Could you explain how floating point numbers are represented in binary.