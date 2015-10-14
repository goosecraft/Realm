import Control.Monad
import System.Time
import System.IO
import Control.Concurrent

-- Derive show because we need to stringify it
data Direction = Forward | Back

-- We can define any behvior for any data this way
instance Show (Direction) where
    show Forward = "вперёд!"
    show Back    = "назад!"

-- Move a dot snake-way
moveDot :: ((Int, Int), Direction) -> ((Int, Int), Direction)
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
keepCrawling :: ((Int, Int), Direction) -> IO ()
keepCrawling position = do
    current <- addDelay position
    putStrLn $ show current
    keepCrawling $ moveDot position

main = keepCrawling start
    where start = ((1, 1), Forward)
