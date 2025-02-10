-- Data Types

-- 1. List
-- [1,2,3,4]

-- 2. Set Comprehension
-- {x^2 | x ∈ {1,2,3,4,5}}
-- {1, 4, 9, 16, 25}

-- 3. List Comprehension
l = [x^2 | x <- [1..5]]
--- [1, 4, 9, 16, 25]

-- [1, 2, 3, 4, 5]
-- [1^2, 2^2, 3^2, 4^2, 5^5]
--- [1, 4, 9, 16, 25]

-- [x**2 for x in range(1,6)]
-- [1, 4, 9, 16, 25]

-- x <- [1..5] is called a generator
list = [(x, y) | x <- [1..3], y <- [4,5]]
-- [(1,4),(1,5),(2,4),(2,5),(3,4),(3,5)]

-- x [1,2,3]
--y [4,5]

-- x = 3
-- y = 4

-- [(1, 4), (1, 5), (2, 4), (2, 5), (3, 4), (3, 5)]

list2 = [(x,y) | y <- [4,5], x <- [1,2,3]]
-- [(1,4),(2,4),(3,4),(1,5),(2,5),(3,5)]

-- for x in [1,2,3]:
--     for y in [4,5]:
--         print(x,y)

list3 = [x | x <- [1..10], even x]

factors :: Int -> [Int]
factors n = [x | x <- [1..n], n `mod` x == 0]

-- [1,2,3,4,5,6,7,8,9,10] --> [1,2,5,10]

-- 5. Type Declaration
-- type String = [Char]

-- 6. Type Synonyms

-- 7. Type Parameters
type Pair a = (a, a)

type Pos = Pair Int

origin :: Pos
origin = (0, 0)

left :: Pos -> Pos
left (x, y) = (x-1,y)

mult :: Pair Int -> Int
mult (m, n) = m * n

copy :: a -> Pair a
copy x = (x, x)

-- 8. Nested Types
type Transpose = Pos -> Pos

-- 9. Data Declarations
-- data Bool = False | True
data Answer = Yes | No | Unknown

answers = [Yes, No, Unknown]

flip :: Answer -> Answer
flip Yes = No
flip No = Yes
flip _ = Unknown

data Shape = Circle Float | Rect Float Float

unitCircle :: Shape
unitCircle = Circle 1.0

unitSquare :: Shape
unitSquare = Rect 1.0 1.0

square :: Float -> Shape
square n = Rect n n

unitSquare2 = square 1.0

area :: Shape -> Float
area (Circle r) = pi * r^2
area (Rect x y) = x * y

-- 10. Data Parameters
-- data Maybe a = Nothing | Just a

safeDiv :: Int -> Int -> Maybe Int
safeDiv _ 0 = Nothing
safeDiv m n = Just (div m n)

divFrom10 :: [Int] -> [Maybe Int]
divFrom10 xs = [safeDiv 10 x | x <- xs]

isJust :: Maybe a -> Bool
isJust (Just _) = True
isJust _ = False

fromJust :: Maybe a -> a
fromJust (Just x) = x
fromJust _ = error "fromJust: Nothing"

extractValues:: [Maybe Int] -> [Int]
extractValues xs = [fromJust x | x <- xs, isJust x]

-- head :: [a] -> a
-- head [] = error "empty list"
-- head (x:_) = x

safeHead :: [a] -> Maybe a
safeHead [] = Nothing
safeHead (x:_) = Just x

inners :: [a] -> [a]
inners [] = []
inners [_] = []
inners xs = init(tail xs)

-- 0..9 A..F = 0 ... 15
-- 0 → 0000 = 0
-- 1 → 0001
-- 2 → 0010
-- 3 → 0011
-- 4 → 0100
-- 5 → 0101
-- 6 → 0110
-- 7 → 0111
-- 8 → 1000
-- 9 → 1001
-- A → 1010
-- B → 1011
-- C → 1100
-- D → 1101 = 13
-- E → 1110
-- F → 1111 = 15

-- Base Case: n = 0, 1, ...
-- Inductive Step: n, n + 1

-- P	   Q    P -> Q
-- True	True	True
-- True	False	False
-- False	True	True
-- False	False	True