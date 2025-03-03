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

sum' :: Num a => [a] -> a
sum' [] = 0
sum' (x:xs) = x + sum' xs

{-
sum [2,3,2]
= sum(2:(3:(2:[])))
= 2 + sum(3:(2:[]))
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

-- sum' :: Num a => [a] -> a
-- sum' [] = 0 -- def. of sum.1
-- sum' (x:xs) = x + sum' xs -- def. of sum.2

{-
  sum [x]
  = sum (x:[]) -- by def. of a list
  = x + sum [] -- by def. of sum.2
  = x + 0 -- by def. of sum.1
  = x -- def. of identity
-}

--
-- 3. Definedness and Termination
--

-- Termination = The function will stop running
-- Definedness = The function returns a result for a given input

-- consider the function for computing factorials
fact :: Integer -> Integer
fact 0 = 1
fact n = n * fact (n-1)

-- fact is not defined for < 0

-- mod :: Integral a => a -> a -> a
-- mod x y = x - y * div x y

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

-- a b o
-- T F F
-- T T T
-- F T T
-- F F T

and :: Bool -> Bool -> Bool
and True True = True
and _ _ = False

-- (The sky is blue) and (the grass is orange)
-- True and False
-- False

or :: Bool -> Bool -> Bool
or False False = False
or _ _ = True

-- (The sky is blue) or (the grass is orange)
-- True or False
-- True

not :: Bool -> Bool
not True = False
not False = True

-- not (The sky is blue)
-- not (True)
-- False

implies :: Bool -> Bool -> Bool
implies True False = False
implies _ _ = True

-- (The sky is blue) implies (the grass is green)
-- True implies True
-- True

-- (The sky is orange) implies (the grass is green)
-- False implies True
-- True

-- Why? The statement only becomes False when the first part is True and the
-- second part is False; otherwise it is ALWAYS True

-- (The sky is blue) implies (the grass is orange)
-- True implies False
-- False

-- Why is this useful? We use it to prove properties mathematically

--
-- 5. Testing Properties with Assumptions
--

-- fact :: Integer -> Integer
-- fact 0 = 1
-- fact n = n * fact (n-1)

factProp :: Integer -> Bool
factProp n = fact n `div` n == fact (n-1)

-- fact 3 `div` 3 == fact 2
-- 3 * 2 * 1 = 6 / 3 = 2
-- 2 * 1 = 2

factProp' :: Integer -> Bool
factProp' n = if n == 0 then True else fact n `div` n == fact (n-1)

factProp'' :: Integer -> Bool
factProp'' n = if n <= 0 then True else fact n `div` n == fact (n-1)

-- a ==> b = case (a,b) of
--   (True, False) -> False
--   _ -> True

-- a b o
-- T F F
-- T T T
-- F T T
-- F F T

factProp''' :: Integer -> Property
factProp''' n = n > 0 ==> fact n `div` n == fact (n-1)

fact :: Integer -> Integer
fact 0 = 1 -- def fact.1
fact n = n * fact (n-1) -- def fact.2

{-
Proof: fact n `div` n == fact (n-1) for n > 0
fact n `div` n
= n * fact (n-1) `div` n -- by def. of fact.2
= fact (n-1) -- by rules of division
-}

--
-- 6. Structural Induction on Lists
--

-- Prove that a logical property P(xs) hold for all finite lists of xs
-- 1. Base Case (xs = []): P([]) holds
-- 2. Inductive Case (xs = x:xs') Prove if P(xs') holds, then P(x:xs') holds

-- P(xs) is known as the induction hypothesis
-- i.e. we assume it to be true and prove P(x:xs')

-- xs = [2,3,4]
-- x = [2]
-- xs' [3,4]

-- P([3,4])
-- P(2 : [3,4])

reverse' :: [a] -> [a]
reverse' [] = [] -- def. of reverse.1
reverse' (x:xs) = reverse' xs ++ [x] -- def. of reverse.2

-- Reasoning about correctness using QuickCheck
reverseProp :: [Integer] -> Bool
reverseProp x = reverse [x] == [x]

{-
Proof
reverse [x]
= reverse (x:[]) -- by def. of a list
= reverse [] ++ [x] -- by def. of reverse.2
= [] ++ [x] -- by def. of reverse.1
= [x] -- by list arithmetic
-}

reverse' :: [a] -> [a]
reverse' [] = [] -- def. of reverse.1
reverse' (x:xs) = reverse' xs ++ [x] -- def. of reverse.2

reverseProp2 :: [Integer] -> Bool
reverseProp2 xs = reverse (reverse xs) == xs

{-
Lemma (assume this to be true for the inductive step):
reverse (xs ++ ys) = reverse ys ++ reverse xs

P(x:xs) = True
P(xs) = True

Inductive Step:
reverse (reverse(x:xs))
= reverse (reverse xs ++ [x]) -- by def. of reverse.2
= reverse [x] ++ reverse (reverse xs) -- by lemma
= [x] ++ reverse (reverse xs) -- by prev proof
= [x] ++ xs -- by inductive hypothesis
= x:xs -- by list arithmetic

Base Case:
reverse (reverse [])
= reverse [] -- by def. of reverse.1
= [] -- by def. of reverse.1
-}

-- Another example

length :: [a] -> Int
length [] = 0 -- def length.1
length (_:xs) = 1 + length xs -- def length.2

-- takes an integer n and returns the first n elements of a list
take' :: Int -> [a] -> [a]
take' _ [] = [] -- def take.1
take' n (x:xs)
  | n > 0 = x : take' (n-1) xs -- def take.2
  | otherwise = [] -- def take.3

takeLengthProp :: [a] -> Bool
takeLengthProp xs = take (length xs) xs == xs

{-
Inductive Step:
take (length (x:xs)) (x:xs) == x:xs
= take (1 + length xs) (x:xs) -- by def. of length.2
= x : take (1 + length xs - 1) (xs) -- by def. of take.2
= x : take (length xs) xs -- by integer arithmetic
= x : xs -- by induction hypothesis

Base Case:
take (length []) []
= take 0 [] -- by definition of length.1
= [] -- by definition of take.1

take (length xs) xs == xs
take (length x:xs) x:xs == x:xs
-}

-- Tips
-- 1. Evaluating integer arithmetic is OK, but not OK for floating point
-- arithmetic because of rounding errors
-- 2. Use Lemmas for trivially true properties
-- 3. Test properties with QuickCheck before trying to prove or assuming as a lemma