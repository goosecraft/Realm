import Control.Monad
import System.Time
import System.IO
import Control.Concurrent

tick = do 
	now <- fmap show getClockTime
	threadDelay $ round $ 1000000
	putStrLn ("Now is: " ++ now)

main = forever $ tick
      
