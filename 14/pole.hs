type Birds = Int
type Pole = (Birds, Birds)

landLeft :: Birds -> Pole -> Either String Pole
landLeft n (left, right)
    | abs ((left + n) - right) < 4 = Right (left + n, right)
    | otherwise                    = Left $ "failed with left: " ++ show (left + n) ++ ", right:" ++ show right

landRight :: Birds -> Pole -> Either String Pole
landRight n (left, right)
    | abs (left - (right + n)) < 4 = Right (left, right + n)
    | otherwise                    = Left $ "failed with left: " ++ show left ++ ", right: " ++ show (right + n)

routine :: Either String Pole
routine = do
    start <- return (0, 0)
    first <- landLeft 2 start
    second <- landRight 3 first
    landLeft 1 second

