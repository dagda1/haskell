import System.Random

turn :: Int -> Int -> Int -> IO ()
turn number attempt attempts
  | attempt == number = putStrLn "You got it"
  | attempts == 0 = putStrLn "You lose"
  | otherwise = do
      let output = if (attempt < number) then "The number is less" else "the number is greater"
      putStrLn output
      guess number attempt attempts

guess :: Int -> Int -> Int -> IO ()
guess number attempt attempts =
  do
    putStr "Try and guess number "
    g <- getLine
    let number' = read g :: Int
    let attempts' = if read g == number
                       then attempts
                       else attempts -1

    turn number number' attempts'

numberGuess :: IO ()
numberGuess = do
  let attempts = 5
  number <- randomRIO (1, 20) :: IO Int
  guess number attempts attempts
