import Data.List
import Data.Char
import qualified Data.Map as Map

-- 6.1
numUniques :: (Eq a) => [a] -> Int
numUniques = length . nub

-- 6.2
wordNums :: String -> [(String, Int)]
wordNums = map (\ws -> (head ws, length ws)) . group . sort . words

isIn :: (Eq a) => [a] -> [a] -> Bool
needle `isIn` haystack = any (needle `isPrefixOf`) (tails haystack)

encode :: Int -> String -> String
encode offset msg = map (\c -> chr $ ord c + offset) msg

decode :: Int -> String -> String
decode shift msg = encode (negate shift) msg

digitSum :: Int -> Int
digitSum = sum . map digitToInt . show

firstTo40 :: Maybe Int
firstTo40 = firstTo 40

firstTo :: Int -> Maybe Int
firstTo n = find (\x -> digitSum x == n) [1..]

-- 6.3
-- phoneBook =
--     [("betty", "555-2938")
--     ,("bonnie", "452-2928")
--     ,("patsy", "493-2928")
--     ,("lucille", "205-2928")
--     ,("wendy", "939-8282")
--     ,("penny", "853-2492")
--     ]

-- findKey :: (Eq k) => k -> [(k, v)] -> Maybe v
-- findKey key [] = Nothing
-- findKey key ((k, v):xs)
--     | key == k  = Just v
--     | otherwise = findKey key xs

findKey :: (Eq k) => k -> [(k, v)] -> Maybe v
findKey key xs = foldr
    (\(k, v) acc -> if key == k then Just v else acc)
    Nothing xs

phoneBook :: Map.Map String String
phoneBook = Map.fromList $
    [("betty", "555-2938")
    ,("bonnie", "452-2928")
    ,("patsy", "493-2928")
    ,("lucille", "205-2928")
    ,("wendy", "939-8282")
    ,("penny", "853-2492")
    ]

string2digits :: String -> [Int]
string2digits = map digitToInt . filter isDigit

