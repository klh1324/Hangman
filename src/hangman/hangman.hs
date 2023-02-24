import System.IO
import System.Random
import Data.Char
import Data.List
import Control.Monad (replicateM)

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
      mapIndices <- (mapToRandomIndices (ceiling (fromIntegral (length word) / fromIntegral 4)) (const '_') (init word))

      play word []  mapIndices
    else if difficulty == "2"
      then do
        (word, hint) <- randomWordAndHint
        mapIndices <- (mapToRandomIndices (ceiling (fromIntegral (length word) / fromIntegral 2)) (const '_') (init word))

        play word []  mapIndices
      else if difficulty == "3"
        then do
          (word, hint) <- randomWordAndHint

          play word [] (replicate ((length word) - 1) '_')
        else if difficulty == "4"
          then do
            (word, hint) <- randomWordAndHint
            play word hint  (replicate ((length word) - 1) '_')
          else do
            putStrLn "Invalid difficulty level. Please try again."
            hangman


mapToRandomIndices :: Int -> (a -> a) -> [a] -> IO [a]
mapToRandomIndices n f xs = do
  let len = length xs
  indices <- replicateM n $ randomRIO (0, len - 1)
  return $ map (\(i, x) -> if i `elem` indices then f x else x) $ zip [0..] xs


-- define the play function
play :: String -> String -> String -> IO ()
play word hint guessed = do
  putStrLn (showHint hint)
  putStrLn (guessed)
  putStrLn "Enter your guess:"
  guess <- getLine
  if isCorrect guess word
    then putStrLn (word ++ "\n" ++ "You win!")
    else if length guess == 1
      then do
        let newGuessed = checkGuess word (head guess) guessed
        if newGuessed == guessed
          then do
            putStrLn "Incorrect guess!"
            play word hint guessed
          else if '_' `notElem` newGuessed
            then putStrLn (newGuessed ++ "\n" ++ "You win!")
            else play word hint newGuessed
      else do
        putStrLn "Invalid guess. Please try again."
        play word hint guessed

-- define the isCorrect function
isCorrect :: String -> String -> Bool
isCorrect guess word = guess == map toLower word


-- define the showHint function
showHint :: String -> String
showHint hint =
  if hint == ""
    then ""
    else "Hint: " ++ hint

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
