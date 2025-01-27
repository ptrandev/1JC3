-- Today's Topics
-- 1. Curried Functions
-- 2. Uncurried Functions
-- 3. Polymorphic Functions
-- 4. Function Composition
-- 5. Type Classes
-- 6. Predefined Classes
-- 7. Overloaded Functions
-- 8. Lambda Expressions
-- 9. Recursion (Map)
-- 10. List Patterns (Refresher)


-- 1. Curried Functions
add :: Int -> Int -> Int
add x y = x + y

add3 = add 3

-- 2. Uncurried Functions
add' (x, y) = x + y

-- 3. Polymorphic Functions
-- a is a type variable, it means "we don't care about the type, it can be anything you want!"
lengthInt :: [Int] -> Int
lengthInt [] = 0
lengthInt (_:xs) = 1 + lengthInt xs

lengthString :: [String] -> Int
lengthString [] = 0
lengthString (_:xs) = 1 + lengthString xs

length' :: [a] -> Int
length' [] = 0
length' (_:xs) = 1 + length' xs

fst' :: (x, y) -> x
fst' (x, _) = x

map' :: (a -> b) -> [a] -> [b]
map' _ [] = []
map' f (x:xs) = f x : map' f xs

-- map' fst' [(1, True), (2, False), (3, True)]

-- map' :: (a -> b) -> [a] -> [b]
-- fst' :: (x, y) -> x

-- (a -> b) = (x, y) -> x

-- [a] = [(x, y)]
-- [b] = [x]

-- a = (x, y)
-- b = x

add1 x = x + 1

result = map' add1 [1,2,3]
-- [1,2,3]
-- [add1 1, add1 2, add1 3]
-- [1 + 1, 2 + 1, 3 + 1]
-- [2,3,4]

-- 4. Function Composition
-- z = f . g


-- reverse :: [a] -> [a]
-- head :: [a] -> a

lastElem :: [a] -> a
lastElem = head . reverse

-- [1,2,3]
-- [3,2,1]
-- 3

lastElem' :: [a] -> a
lastElem' xs = head (reverse xs)

f (x:xs) = foldr ((+) . (+2)) 0 (tail (reverse (x:xs)))
f' (x:xs) = foldr ((+) . (+2)) 0 $ tail $ reverse $ x:xs

-- foldr (+) 5 [1,2,3,4]
-- 5 + 4 = 9 + 3 = 12 + 2 = 14 + 1 = 15

-- 5. Type Classes

class Eq' a where
  (==) :: a -> a -> Bool
  (/=) :: a -> a -> Bool


instance Eq' Bool where
  True == True = True
  False == False = True
  _ == _ = False

  x /= y = not (x Prelude.== y)

instance Eq' Int where
  x == y = x Prelude.== y
  x /= y = not (x Prelude.== y)

class Example a where
  ex :: a -> a

instance Example Int where
  ex x = x + 1

instance Example Bool where
  ex x = not x

-- 6. Predefined Classes

-- 7. Overloaded Functions
sum' :: Num a => [a] -> a
sum' [] = 0
sum' (x:xs) = x + sum' xs

divide' :: Float -> Float -> Float
divide' x y = x / y

divide'' :: Double -> Double -> Double
divide'' x y = x / y

divide''' :: Fractional a => a -> a -> a
divide''' x y = x / y

-- divide''' 2.0 3.0 :: Float
-- Fractional a => a -> a -> a
-- Float -> Float -> Float

-- 8. Lambda Expressions

triple :: Int -> Int
triple x = x + x + x
triple' = (\x -> x + x + x)

add'' :: Int -> Int -> Int
add'' x y = x + y
add''' :: Int -> Int -> Int
add''' = \x -> (\y -> x + y)

add1' :: Int -> Int
add1' x = x + 1

sum1 :: [Int] -> [Int]
sum1 xs = map add1 xs

sum1' :: [Int] -> [Int]
sum1' xs = map (\x -> x + 1) xs

-- 8. Recursion

-- [1,2,3]
-- 1:2:3:[]

-- 9. List Patterns
head :: [a] -> a
head [] = error "Empty"
head (x:xs) = x

tail' :: [a] -> [a]
tail' [] = []
tail' (x:xs) = xs