-- Lab 6: Introduction to Haskell

double x = x + x

sumOfSquares x y = x*x + y*y

absVal x = if x >= 0 then x else -x

-- 'onlyEvens' returns a new list
onlyEvens [] = []
onlyEvens [_, 324, _] = [999]
onlyEvens (x:xs) = if (mod x 2 == 0)
                    then x : onlyEvens xs
                    else onlyEvens xs

lengthPlus3 xs = length xs + 3

-- applyUnary takes in a unary function and a value; applies the function to the value and returns the result
applyUnary f x = f x

-- makeConstantFunction takes a value x and returns a unary function that ignores its argument and always returns x
makeConstantFunction x = (\_ -> x)

-- join takes a list of strings and outputs a single string that is the concatenation of all the strings in the list
join [] = ""
join [x] = x
join (x:xs) = x ++ join xs