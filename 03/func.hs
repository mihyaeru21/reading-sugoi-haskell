{-# OPTIONS -Wall -Werror #-}

-- 3.1
factorial :: Integer -> Integer
factorial 0 = 1
factorial n = n * factorial (n - 1)

charName :: Char -> String
charName 'a' = "AAAAA"
charName _   = "hoge"

addVectors :: (Double, Double) -> (Double, Double) -> (Double, Double)
addVectors (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)

first :: (a, b, c) -> a
first (x, _, _) = x

second :: (a, b, c) -> b
second (_, y, _) = y

third :: (a, b, c) -> c
third (_, _, z) = z

head' :: [a] -> a
head' [] = error "error!!!"
head' (x:_) = x

tell :: (Show a) => [a] -> String
tell [] = "The list is empty"
tell (x:[]) = "The list has one element: " ++ show x
tell (x:y:[]) = "The list has two elements: " ++ show x ++ " and " ++ show y
tell (x:y:_) = "This list is log. The first two elements are: " ++ show x ++ " and " ++ show y

firstLetter :: String -> String
firstLetter "" = "Empty string!!"
firstLetter hoge@(x:_) = "The first letter of " ++ hoge ++ " is " ++ [x]

-- 3.2
bmiTell :: Double -> String
bmiTell bmi
    | bmi <= 18.5 = "You're underweight"
    | bmi <= 25.0 = "You're supposedly normal"
    | bmi <= 30.0 = "You're fat!"
    | otherwise   = "You're a whale!!!"

bmiTell' :: Double -> Double -> String
bmiTell' weight height
    | weight / height ^ (2 :: Int) <= 18.5 = "You're underweight"
    | weight / height ^ (2 :: Int) <= 25.0 = "You're supposedly normal"
    | weight / height ^ (2 :: Int) <= 30.0 = "You're fat!"
    | otherwise   = "You're a whale!!!"

max' :: (Ord a) => a -> a -> a
max' a b
    | a <= b    = b
    | otherwise = a

compare' :: (Ord a) => a -> a -> Ordering
a `compare'` b
    | a == b    = EQ
    | a <  b    = LT
    | otherwise = GT

-- 3.3
bmiTell'' :: Double -> Double -> String
bmiTell'' weight height
    | bmi <= 18.5 = "You're underweight"
    | bmi <= 25.0 = "You're supposedly normal"
    | bmi <= 30.0 = "You're fat!"
    | otherwise   = "You're a whale!!!"
    where bmi = weight / height ^ (2 :: Int)

bmiTell''' :: Double -> Double -> String
bmiTell''' weight height
    | bmi <= skinny = "You're underweight"
    | bmi <= normal = "You're supposedly normal"
    | bmi <= fat    = "You're fat!"
    | otherwise   = "You're a whale!!!"
    where bmi = weight / height ^ (2 :: Int)
          skinny = 18.5
          normal = 25.0
          fat    = 30.0

calcBmis :: [(Double, Double)] -> [Double]
calcBmis xs = [bmi | (w, h) <- xs, let bmi = w / h ^ (2 :: Int)]

calcBmis' :: [(Double, Double)] -> [Double]
calcBmis' xs = [bmi | (w, h) <- xs, let bmi = w / h ^ (2 :: Int), bmi > 25.0]

-- 3.5
head'' :: [a] -> a
head'' xs = case xs of [] -> error "empty!!"
                       (x:_) -> x

describeList :: [a] -> String
describeList ls = "The list is "
                  ++ case ls of [] -> "empty."
                                [_] -> "a singleton list."
                                _ -> "a longer list."

describeList' :: [a] -> String
describeList' ls = "The list is " ++ what ls
    where what [] = "empty."
          what [_] = "a singleton list."
          what _ = "a longer list."

