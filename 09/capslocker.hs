import Control.Monad (forever)
import Data.Char (toUpper)

main = forever $ do
    l <- getLine
    putStrLn $ map toUpper l

