-- Table of Contents
-- 1. Type Review
-- 2. Overloading

--
-- 1. Type Review
--

-- because addition is defined for all numbers
obviouslyNum :: Num a => a -> a
obviouslyNum x = x + 1

-- must ensure equality is defined
noticeTheEquals :: Eq a => a -> a -> Char
noticeTheEquals x y = if x == y then 'a' else 'b'

-- a fraction is also a number
fractionalAnyone :: Fractional a => a -> a
fractionalAnyone f = f * (1/f)

-- this can also be defined as a Num
one :: Integer
one = 1

--
-- 2. Overloading
--

-- All the previous functions are overloaded, which means they can take
-- different types. This is because they are polymorphic. The type
-- variables a, b, c, etc. are not bound to a specific type. They can
-- be any type that satisfies the constraints.

-- What's the difference between polymorphism and overloading?
-- Polymorphism is when a function can take different types, while
-- overloading is when a function can take different types and
-- perform different operations on them.

-- A standard polymorphic function
length' :: [a] -> Int
length' [] = 0
length' (_:xs) = 1 + length' xs

-- Overloaded functions are implemented via Type classes

-- interfaces... this specifies the default behaviour
class Eq a where
  (==) :: a -> a -> Bool
  (/=) :: a -> a -> Bool
  x /= y = not (x == y)
  x == y = not (x /= y)

-- and instances...
-- different definition for each type
instance Eq Bool where
  True == True = True
  False == False = True
  _ == _ = False

instance Eq a => Eq (Maybe a) where
  Nothing == Nothing = True
  (Just x) == (Just y) = x == y
  _ == _ = False


--
-- 3. Type Class Interface
--

-- interfaces specify a collection of type signatures over a type parameter

-- For instance, the Functor class define the map function for data types
-- other than lists.

-- we define the type class name (Functor)
-- and the type parameter f, which is the type constructor
-- and the function signature for fmap
class Functor f where
  fmap :: (a -> b) -> f a -> f b

-- now we write instances to define how this function works for different types
instance Functor Maybe where
  fmap _ Nothing = Nothing
  fmap f (Just x) = Just (f x)

instance Functor (Either a) where
  fmap f (Left x) = Left x -- keep the Left value unchanged, to match the type
  fmap f (Right x) = Right (f x) -- apply f to the Right value, to match the type

data Maybe a = Nothing | Just a deriving (Eq, Show)
data Either a b = Left a | Right b deriving (Eq, Show)


--
-- 4. Vector Spaces
--

--A Vector Space is a collection of functions that would need to be defined
-- differently given a Vector definition (e.g. 2D, 3D, etc.) while obeying certain laws

-- many computations can be performed in terms of vector space operations
-- i.e. regardless of underlying vector data type
-- how can we develop a library to take advantage of this generalization

vecZero :: Vector
vecScalarProd :: Double -> Vector -> Vector
vecSum :: Vector -> Vector -> Vector
vecMagnitude :: Vector -> Double
vecInnerProd :: Vector -> Vector -> Double

-- Step 1: Create interface that defined collection of core operations that
-- access the Vector type directly
class VectorSpace v where
  vecZero :: (Num a) => v a
  vecSum :: (Num a) => v a -> v a -> v a
  vecScalarProd :: (Num a) => a -> v a -> v a
  vecMagnitude :: (Floating a) => v a -> a
  vecInnerProd :: (Num a) => v a -> v a -> a

data Vector2 a = Vector2 (a, a) deriving (Eq, Show)

-- we define how these work for a 2D vector
instance VectorSpace Vector2 where
  vecZero = Vector2 (0, 0)
  vecSum (Vector2 (x1, y1)) (Vector2 (x2, y2)) = Vector2 (x1 + x2, y1 + y2)
  vecScalarProd s (Vector2 (x, y)) = Vector2 (s * x, s * y)
  vecMagnitude (Vector2 (x, y)) = sqrt (x^2 + y^2)
  vecInnerProd (Vector2 (x1, y1)) (Vector2 (x2, y2)) = x1 * x2 + y1 * y2

data Vector3 a = Vector3 (a, a, a) deriving (Eq, Show)

-- we define how these work for a 3D vector
instance VectorSpace Vector3 where
  vecZero = Vector3 (0, 0, 0)
  vecSum (Vector3 (x1, y1, z1)) (Vector3 (x2, y2, z2)) = Vector3 (x1 + x2, y1 + y2, z1 + z2)
  vecScalarProd s (Vector3 (x, y, z)) = Vector3 (s * x, s * y, s * z)
  vecMagnitude (Vector3 (x, y, z)) = sqrt (x^2 + y^2 + z^2)
  vecInnerProd (Vector3 (x1, y1, z1)) (Vector3 (x2, y2, z2)) = x1 * x2 + y1 * y2 + z1 * z2

-- Step 2: Create library that uses the VectorSpace interface
-- Now, these functions can be used with any vector type that implements the
-- VectorSpace interface. This allows us to write generic code that works
-- with any vector type, without having to worry about the specific implementation.
vecDiff:: (VectorSpace v, Num a) => v a -> v a -> v a
vecDiff v1 v2 = vecSum v1 (vecScalarProd (-1) v2) -- sum of the first vector and second vector negated

vecDist :: (VectorSpace v, Floating a) => v a -> v a -> a
vecDist v1 v2 = vecMagnitude (vecDiff v1 v2) -- magnitude of the difference between the two vectors

vecMax :: (VectorSpace v, Ord a) => v a -> v a -> v a
vecMax v1 v2 = if vecMagnitude v1 > vecMagnitude v2 then v1 else v2 -- return the vector with the largest magnitude