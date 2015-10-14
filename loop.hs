import Control.Concurrent
import Control.Monad
import Data.List
import System.Time
import System.IO

-- Alias
type Point = (Int, Int)

data Direction = Forward | Back

-- We can define any behvior for any data this way
instance Show (Direction) where
    show Forward = "вперёд!"
    show Back    = "назад!"

-- Move a dot snake-way
moveDot :: (Point, Direction) -> (Point, Direction)
moveDot ((x, y), _)       | x == 10 && y == 10 = ((1, 1), Forward)
moveDot ((x, y), Forward) | x == 10            = ((x, y + 1), Back)
moveDot ((x, y), Back)    | x == 1             = ((x, y + 1), Forward)
moveDot ((x, y), Forward)                      = ((x + 1, y), Forward)
moveDot ((x, y), Back)                         = ((x - 1, y), Back)

-- always return same given value but with delay
addDelay :: Show a => a -> IO a
addDelay x = fmap constant delay
    where delay      = threadDelay 1000000
          constant _ = x

-- output position and move forward
keepCrawling :: (Point, Direction) -> IO ()
keepCrawling position = do
    current <- addDelay position
    putStrLn $ show current
    keepCrawling $ moveDot position

-- output sqare field
showField :: Int -> Char -> String
showField i c = (intercalate "\n" $ getField i c) ++ "\n"
    where getField i c = [getLine i c | _ <- [1..i]]
          getLine  i c = [c | _ <- [1..i]]

main = keepCrawling start
    where start = ((1, 1), Forward)
