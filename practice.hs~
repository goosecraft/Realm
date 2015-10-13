import System.IO

main :: IO ()
main = do
	consoleQuestion
	question

consoleQuestion :: IO ()
consoleQuestion = do
		putStrLn ("Hello, Stranger! " ++ "What is your name?")
		name <- getLine
		putStrLn ("Nice to meet you, " ++ name)
		
question :: IO ()
question = do
	putStrLn "Wanna big heap of money?"
	answer <- getLine
	if (checkAnswer answer)
	then putStrLn "You will have it ... soon"
	else putStrLn "Really?"

checkAnswer :: [Char] -> Bool
checkAnswer answer
	| answer == "yes" = True
	| otherwise = False



