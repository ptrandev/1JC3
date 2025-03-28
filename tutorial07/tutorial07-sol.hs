-- Table of Contents
-- 1. Ways to Reason
-- 2. Proving Properties
-- 3. Definedness and Termination
-- 4. Implication
-- 5. Testing Properties with Assumptions
-- 6. Structural Induction on Lists

-- Note: Just a primer on reasoning about programs, not a comprehensive guide
-- Don't worry if you don't understand everything
-- May help you understand how to test programs and prove properties in the
-- future


-- cabal install --lib --package-env . QuickCheck
import Test.QuickCheck

--
-- 1. Ways to Reason
--

-- How can we reason about what a program does?
-- 1. Evaluate it for particular inputs in GHCi
-- 2. Do the same by hand, with line-by-line calculation
-- 3. Or we can reason about how the program behaves in general

-- consider a simple function definition by pattern matching

sum' :: Num a => [a] -> a
sum' [] = 0 -- def sum.1
sum' (x:xs) = x + sum' xs -- def sum.2

-- sum [2,3,2] = 7

{-
  sum [2,3,2]
  = sum (2:(3:(2:[])))
  = 2 + sum (3:(2:[]))
  = 2 + 3 + sum (2:[])
  = 2 + 3 + 2 + sum []
  = 2 + 3 + 2 + 0
  = 7
-}

-- reason about how the function should behave by defining a property that
-- should hold
sumOneProp :: Integer -> Bool
sumOneProp x = sum [x] == x

--
-- 2. Proving Properties
--

-- let us prove a certain property by reasoning about case definitions

{-
sum [x]
= sum (x:[]) -- by def. of a list
= x + sum [] -- by def. of sum.2
= x + 0 -- by def. of sum.1
= x -- integer arithmetic
-}

--
-- 3. Definedness and Termination
--

-- Termination = the function will stop running
-- Definedness = the function will return a result for the given input

-- consider the function for computing factorials
fact :: Integer -> Integer
fact 0 = 1
fact n = n * fact (n-1)

-- try entering...
-- fact (-1) -- -1 is NOT DEFINED for fact and will not terminate

mod :: Integral a => a -> a -> a
mod x y = x - y * div x y

-- try entering...
-- mod 5 0 -- 0 is NOT DEFINED for mod

-- so when you're proving these algorithms, there may be some inputs that are
-- not defined for the function, and the function may not terminate for some
-- inputs

-- so you may need to prove that the function is defined for the inputs you
-- care about, and that it terminates for those inputs

-- For instance, mod will not terminate for an input with a divisor of 0
-- and fact will not terminate for a negative input

--
-- 4. Implication
--

-- the logical if-then
-- i.e. a => b specifies in a is True then b must also be True

-- a ==> b = case (a,b) of
--   (True, False) -> False
--   _ -> True

-- a === b = (not a) || b

-- Prepositional Logic

and :: Bool -> Bool -> Bool
and True True = True
and _ _ = False

-- (The sky is blue) and (the grass is orange)
-- False

or :: Bool -> Bool -> Bool
or False False = False
or _ _ = True

-- (The sky is blue) or (the grass is orange)
-- True

not :: Bool -> Bool
not True = False
not False = True

-- not (The sky is blue)
-- False

implies :: Bool -> Bool -> Bool
implies True False = False
implies _ _ = True

-- if (The sky is blue) then (the grass is green)
-- (The sky is blue) implies (the grass is green)
-- True

-- (The sky is orange) implies (the grass is green)
-- True
-- Why? The statement only becomes False when the first part is True and the
-- second part is False; otherwise it is ALWAYS True

-- (The sky is blue) implies (the grass is orange)
-- False

-- Why is this useful? We use it to prove properties mathematically

--
-- 5. Testing Properties with Assumptions
--

-- fact :: Integer -> Integer
-- fact 0 = 1
-- fact n = n * fact (n-1)


-- the following QuickCheck property for fact
factProp :: Integer -> Bool
factProp n = fact n `div` n == fact (n-1)

-- we get a divide by zero error
-- quickCheck factProp

-- so we gotta modify the property to exclude the case where n = 0
factProp' :: Integer -> Bool
factProp' n = if n == 0 then True else fact n `div` n == fact (n-1)

-- we get a stack overflow... why?
-- we have a negative number as a test case, we need to exclude that as well

factProp'' :: Integer -> Bool
factProp'' n = if n <= 0 then True else fact n `div` n == fact (n-1)

-- now it passes!

-- we can also use the implication operator to exclude certain cases
factProp''' :: Integer -> Property
factProp''' n = n >= 0 ==> fact n `div` n == fact (n-1)

-- Proving factprop

-- fact :: Integer -> Integer
-- fact 0 = 1 -- def fact.1
-- fact n = n * fact (n-1) -- def fact.2

{-
Proof (n >= 1):
fact n `div` n
= (n * fact (n-1)) `div` n -- by def. of fact.2
= fact (n-1) -- by integer arithmetic, since n / n
-}

--
-- 6. Structural Induction on Lists
--

-- Prove that a logical property P(xs) hold for all finite lists xs, we must...
-- Base Case (xs = []): Prove that P([]) holds
-- Inductive Case (xs = x:xs'): Prove that if P(xs') holds, then P(x:xs') holds

-- P(xs) is known as the induction hypothesis
-- i.e. we assume it to be true and prove P(x:xs)

reverse' :: [a] -> [a]
reverse' [] = []
reverse' (x:xs) = reverse' xs ++ [x]

-- Reasoning about correctness using QuickCheck
reverseProp :: [Integer] -> Bool
reverseProp x = reverse [x] == [x]

{-
Proof
reverse [x]
= reverse (x:[]) -- by def. of list
= reverse [] ++ [x] -- by def. of reverse.2
= [] ++ [x] -- by def. of reverse.1
= [x] -- by list arithmetic
-}

reverseProp2 :: [Integer] -> Bool
reverseProp2 xs = reverse (reverse xs) == xs

{-
Base Case:
reverse (reverse [])
= reverse [] -- by def. of reverse.1
= [] -- by def. of reverse.1

Lemma (assume this to be true for the inductive step):
reverse (xs ++ ys) = reverse ys ++ reverse xs

-- Inductive Step:
reverse (reverse (x:xs))
= reverse (reverse xs ++ [x]) -- by def. of reverse.2
= reverse [x] ++ reverse (reverse xs) -- by lemma
= [x] ++ reverse (reverse xs) -- by prev proof
= [x] ++ xs -- by induction hypothesis
= x:xs -- by list arithmetic
-}

-- Another example

length :: [a] -> Int
length [] = 0
length (_:xs) = 1 + length xs

-- takes an integer n and returns the first n elements of a list
take' :: Int -> [a] -> [a]
take' _ [] = [] -- def take.1
take' n (x:xs)
  | n > 0 = x : take' (n-1) xs -- def take.2
  | otherwise = [] -- def take.3

takeLengthProp :: [a] -> Bool
takeLengthProp xs = take (length xs) xs == xs

-- can we prove this property of both take and length? Yes, using structural induction

{-
Base Case:
take (length []) []
= take 0 [] -- by def. of length.1
= [] -- by def. of take.1

Inductive Step:
take (length (x:xs)) (x:xs)
= take (1 + length xs) (x:xs) -- by def. of length.2
= x : take (1 + length xs - 1) xs -- by def. of take.2
= x : take (length xs) xs -- by integer arithmetic
= x : xs -- by induction hypothesis
-}

-- Tips
-- 1. Evaluating integer arithmetic is OK, but not OK for floating point
-- arithmetic because of rounding errors
-- 2. Use Lemmas for trivially true properties
-- 3. Test properties with QuickCheck before trying to prove or assuming as a lemma