import System.IO
import System.Random
import Data.Char

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
      play word []
    else if difficulty == "2"
      then do
        (word, hint) <- randomWordAndHint
        play word []
      else if difficulty == "3"
        then do
          (word, hint) <- randomWordAndHint
          play word []
        else if difficulty == "4"
          then do
            (word, hint) <- randomWordAndHint
            play word hint
          else do
            putStrLn "Invalid difficulty level. Please try again."
            hangman

-- define the play function
play :: String -> String -> IO ()
play word hint = do
 -- let showGuess = map (\c -> if c `elem` guessedLetters then c else '_') word
  --where guessedLetters = guess
  putStrLn (showHint hint)
  putStrLn (showGuess word)
  putStrLn "Enter your guess:"
  guess <- getLine
  --if isCorrect guess word
  if guess == word
    then putStrLn (word ++ "\n" ++ "You win!")
    else if length guess == 1
      then do
        let newWord = checkGuess word (head guess) -- fix here
        if newWord /= word
          then do
            putStrLn "Incorrect guess!"
            play word hint
          else if newWord == map toLower word
            then putStrLn (word ++ "\n" ++ "You win!")
            else play newWord hint
      else do
        putStrLn "Invalid guess. Please try again."
        play word hint

-- define the isCorrect function
isCorrect :: String -> String -> Bool
isCorrect guess word = guess == map toLower word


-- define the showHint function
showHint :: String -> String
showHint hint =
  if hint == ""
    then ""
    else "Hint: " ++ hint

-- define the showGuess function
showGuess :: String -> String
showGuess word =
  if word == ""
    then ""
    else "_" ++ ((tail word))
--showGuess word = map (\c -> if c `elem` guessedLetters then c else '_') word
  --where guessedLetters = tail word

-- define the checkGuess function
checkGuess :: String -> Char -> String
checkGuess word guess
  | word == ""   = ""
  | head word == guess = guess : (checkGuess (tail word) guess)
  | otherwise    = (head word) : (checkGuess (tail word) guess)

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
