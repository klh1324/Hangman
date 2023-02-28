-- Ues the following commands to run the program
-- ghci
-- :set -package aeson
-- :l hangman.hs
-- hangman

import System.IO
import System.Random
import Data.Char
import Data.List
import Control.Monad (replicateM)

import GHC.Generics (Generic)
import Data.Aeson (FromJSON, ToJSON, encode, decode)
import qualified Data.ByteString.Lazy as B
import GHC.Utils.Binary (newReadState)

--------- Definitions ---------

-- Number of guesses allowed
guessLimit :: Integer
guessLimit = 7

-- Mapping of the word with its corresponding hint
type WordAndHint = (String, String)

-- Mapping of the indices of the word with its corresponding character
type MapIndices = String

-- MetaData Storage for all past results: List of Strings (guessed words) and booleans (result it won or lost)
data MetaResult = MetaResult { pastResults :: [(String, Bool)]} deriving (Show, Eq, Generic)


data Result = 
  Continue Int Int String String | 
  Won Int Int String |
  Lose Int String 


--------- Main Functions ---------

-- define the hangman game
hangman :: IO MetaResult
hangman = do
  metaResults <- loadMetaResults
  run metaResults
  

run :: MetaResult -> IO MetaResult
run metaResults = do
  newResults <- initGame metaResults
  shouldContinue <- continuePlayGame
  if shouldContinue
    then run newResults 
  else 
    return newResults

-- define the continuePlayGame
continuePlayGame :: IO Bool
continuePlayGame = do
  putStrLn "Do you want to continue?(y/n)"
  response <- getLine
  case response of
    "y" -> return True
    "n" -> return False
    _ -> do
      putStrLn "Invalid response. Please enter 'y' or 'n'."
      continuePlayGame      

-- define the initGame function
initGame :: MetaResult -> IO MetaResult
initGame metaResults = do
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
      play word []  mapIndices guessLimit metaResults
    else if difficulty == "2"
      then do
        (word, hint) <- randomWordAndHint
        mapIndices <- (mapToRandomIndices (ceiling (fromIntegral (length word) / fromIntegral 2)) (const '_') (word))
        play word []  mapIndices guessLimit metaResults
      else if difficulty == "3"
        then do
          (word, hint) <- randomWordAndHint
          play word [] (replicate ((length word)) '_') guessLimit metaResults
        else if difficulty == "4"
          then do
            (word, hint) <- randomWordAndHint
            play word hint  (replicate ((length word)) '_') guessLimit metaResults
          else do
            putStrLn "Invalid difficulty level. Please try again."
            hangman
    

-- define the play function
play :: String -> String -> MapIndices -> Integer -> MetaResult -> IO MetaResult
play word hint guessed remainingGuess metaResults = do
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
            then play word hint guessed (remainingGuess - 1) metaResults
          else do
            let newResult = addLostGame word metaResults
            putStrLn "Sorry! No more guesses remaining\n"
            return newResult
      else if '_' `notElem` newGuessed -- correct guess and player wins
        then do
          let newResult = addWonGame word metaResults
          putStrLn ("Correct! you have guessed the word " ++ word ++ "!\n" ++ "You win!")
          return newResult
      else 
        do
        putStrLn ("Correct guess! " ++ show remainingGuess ++ " guesses remaining!")
        play word hint newGuessed remainingGuess  metaResults
  else do
    putStrLn "Invalid guess. You must enter a lowercase charachter. Please try again."
    play word hint guessed remainingGuess metaResults

--------- MetaResult Functions ---------

loadMetaResults :: IO MetaResult 
loadMetaResults = do
  return (MetaResult [])

addWonGame :: String -> MetaResult -> MetaResult
addWonGame word (MetaResult results) = (MetaResult ((word, True):results))

addLostGame :: String -> MetaResult -> MetaResult
addLostGame word (MetaResult results) = (MetaResult ((word, False):results))

-- getSize (MetaResult results) = length results
-- printSz len = do 
--   putStrLn(show len)
--   return ()

--------- Helper Functions ---------

-- define the mapToRandomIndices function
-- Map the function f to a certain number of characters n in the word xs with indices picked at random
mapToRandomIndices :: Int -> (a -> a) -> [a] -> IO [a]
mapToRandomIndices n f xs = do
  let len = length xs
  indices <- replicateM n $ randomRIO (0, len - 1)
  return $ map (\(i, x) -> if i `elem` indices then f x else x) $ zip [0..] xs


-- define the isCorrect function
isCorrect :: String -> String -> Bool
isCorrect guess word = guess == map toLower word


-- define the isValidGuess function
-- Check if the guess is valid
isValidGuess :: String -> Bool
isValidGuess (h:t) = null t && isLower h
isValidGuess "" = False


-- define the showHint function
-- Show the hint of the word if it exists for this difficulty level
showHint :: String -> String
showHint hint =
  if hint == ""
    then ""
    else "Hint: " ++ hint ++ "\n"


-- define the checkGuess function
-- Check the guess against the word and update 'guessed' accordingly 
checkGuess :: String -> Char -> MapIndices -> MapIndices
checkGuess word guess guessed =  
  [if c == guess && g == '_' then c else g | (c, g) <- zip word guessed]
  

-- define the randomWordAndHint function
-- Get a random word and its corresponding hint from the txt files
randomWordAndHint :: IO WordAndHint
randomWordAndHint = do
  wordList <- readFile "wordlist.txt"
  hintList <- readFile "hintlist.txt"
  let words = lines wordList
      hints = lines hintList
  index <- randomRIO (0, length words - 1)
  let word = words !! index
      hint = hints !! index
  return (word, hint)