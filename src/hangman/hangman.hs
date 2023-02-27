import System.IO
import System.Random
import Data.Char
import Data.List
import Control.Monad (replicateM)

guessLimit = 7

-- define the hangman game
hangman :: IO ()
hangman = do
  putStrLn "Welcome to Hangman! Choose a difficulty level:"
  putStrLn "1. Easy"
  putStrLn "2. Medium"
  putStrLn "3. Hard"
  putStrLn "4. English as a Second Language (ESL)"
  difficulty <- getLine
  if difficulty == "1"
    then do
      (word, hint) <- randomWordAndHint
      mapIndices <- (mapToRandomIndices (ceiling (fromIntegral (length word) / fromIntegral 4)) (const '_') (word))
      play word []  mapIndices guessLimit
    else if difficulty == "2"
      then do
        (word, hint) <- randomWordAndHint
        mapIndices <- (mapToRandomIndices (ceiling (fromIntegral (length word) / fromIntegral 2)) (const '_') (word))
        play word []  mapIndices guessLimit
      else if difficulty == "3"
        then do
          (word, hint) <- randomWordAndHint
          play word [] (replicate ((length word)) '_') guessLimit
        else if difficulty == "4"
          then do
            (word, hint) <- randomWordAndHint
            play word hint  (replicate ((length word)) '_') guessLimit
          else do
            putStrLn "Invalid difficulty level. Please try again."
            hangman


mapToRandomIndices :: Int -> (a -> a) -> [a] -> IO [a]
mapToRandomIndices n f xs = do
  let len = length xs
  indices <- replicateM n $ randomRIO (0, len - 1)
  return $ map (\(i, x) -> if i `elem` indices then f x else x) $ zip [0..] xs


-- define the play function
play :: String -> String -> String -> Integer -> IO ()
play word hint guessed remainingGuess = do
  -- putStrLn word
  -- putStrLn guessed
  putStr (showHint hint)
  putStrLn guessed
  putStrLn ("You have " ++ show remainingGuess ++ " guesses remaning. Enter your guess:")
  guess <- getLine
  if isValidGuess guess
    then do
      let newGuessed = checkGuess word (head guess) guessed
      if newGuessed == guessed -- incorrect guess
        then do
          putStrLn ("Incorrect guess! " ++ show (remainingGuess - 1) ++ " guesses remaining!")
          if (remainingGuess - 1) > 0 
            then play word hint guessed (remainingGuess - 1) 
          else putStrLn "Sorry! No more guesses remaining\n"
      else if '_' `notElem` newGuessed -- correct guess and player wins
        then do
          putStrLn ("Correct! you have guessed the word " ++ word ++ "!\n" ++ "You win!")
      else 
        do
        putStrLn ("Correct guess! " ++ show remainingGuess ++ " guesses remaining!")
        play word hint newGuessed remainingGuess 
  else do
    putStrLn "Invalid guess. You must enter a lowercase charachter. Please try again."
    play word hint guessed remainingGuess

-- define the isCorrect function
isCorrect :: String -> String -> Bool
isCorrect guess word = guess == map toLower word

-- define the isValidGuess function
isValidGuess :: String -> Bool
isValidGuess (h:t) = null t && isLower h
isValidGuess "" = False

-- define the showHint function
showHint :: String -> String
showHint hint =
  if hint == ""
    then ""
    else "Hint: " ++ hint ++ "\n"

checkGuess :: String -> Char -> String -> String
checkGuess word guess guessed =  
  [if c == guess && g == '_' then c else g | (c, g) <- zip word guessed]
  
-- define the randomWordAndHint function
randomWordAndHint :: IO (String, String)
randomWordAndHint = do
  wordList <- readFile "wordlist.txt"
  hintList <- readFile "hintlist.txt"
  let words = lines wordList
      hints = lines hintList
  index <- randomRIO (0, length words - 1)
  let word = words !! index
      hint = hints !! index
  return (word, hint)
