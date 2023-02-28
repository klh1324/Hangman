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
import Text.Read (readMaybe)
import Data.List
import System.IO.Unsafe (unsafePerformIO)

import GHC.Generics (Generic)
import Data.Aeson (FromJSON, ToJSON, encode, decode)
import qualified Data.ByteString.Lazy as B
import GHC.Utils.Binary (newReadState)



--------- Definitions ---------

-- Number of guesses allowed
guessLimit :: Integer
guessLimit = 7

-- File paths of json file used for read/write results
jsonfile :: FilePath
jsonfile = "result.json"

-- File paths for the list of words
wordfile :: FilePath
wordfile = "wordlist.txt"

-- File path for the list of hints
hintfile :: FilePath
hintfile = "hintlist.txt"

-- Mapping of the word with its corresponding hint
type WordAndHint = (String, String)

-- Mapping of the indices of the word with its corresponding character
type MapIndices = String

-- MetaData Storage for all past results: List of Strings (guessed words) and booleans (result it won or lost)
data MetaResult = MetaResult { pastResults :: [(String, Bool)]} deriving (Show, Eq, Generic)
instance FromJSON MetaResult
instance ToJSON MetaResult

-- data for hangmanGame
data HangmanGame = HangmanGame
  { word :: String,
    hint :: String,
    guessed :: MapIndices,
    remainingGuess :: Integer,
    metaResults :: MetaResult 
  }
  
--data for DifficultyLevel
data DifficultyLevel = DifficultyLevel
  { levelName :: String,
    hint_d :: String,
    getMapIndices :: String -> [Char]
  }



--------- Main Functions ---------

-- main function to start program
hangman :: IO ()
hangman = do
  savedResults <- loadMetaResults
  updatedResults <- run savedResults
  putStrLn "Thank you for playing!"
  displayResults updatedResults
  saveMetaResults updatedResults

-- run function that continuously create new games for user
run :: MetaResult -> IO MetaResult
run metaResults = do
  newResults <- initGame metaResults
  shouldContinue <- continuePlayGame
  if shouldContinue
    then run newResults 
  else 
    return newResults
        

-- define the initGame function
initGame :: MetaResult -> IO MetaResult
initGame metaResults = do
  putStrLn "Welcome to Hangman! Choose a difficulty level:"
  levels <- difficultyLevels
  mapM_ (\(i, level) -> putStrLn (show i ++ ". " ++ levelName level)) (zip [1 ..] levels)
  difficulty <- getLine
  let difficultyLevel = readMaybe difficulty >>= \n -> if n >= 1 && n <= length levels then Just (levels !! (n - 1)) else Nothing
  case difficultyLevel of
    Just level -> do
      (word, hint) <- randomWordAndHint
      mapIndices <- return (getMapIndices level word)
      let gameState = HangmanGame word (hint_d level) mapIndices guessLimit metaResults
      play gameState
    Nothing -> do
      putStrLn "Invalid difficulty level. Please try again."  
      initGame
       metaResults
      
-- define the play function
play :: HangmanGame -> IO MetaResult
play (HangmanGame word hint guessed remainingGuess metaResults) = do
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
            then play (HangmanGame word hint guessed (remainingGuess - 1) metaResults)
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
        play (HangmanGame word hint newGuessed remainingGuess metaResults)
  else do
    putStrLn "Invalid guess. You must enter a lowercase charachter. Please try again."
    play (HangmanGame word hint guessed remainingGuess metaResults)



--------- IO Helper Functions ---------

-- function that asks user if they want to continue or not and returns IO boolean  
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

-- function that loads the MetaResults from saved JSON file 
loadMetaResults :: IO MetaResult
loadMetaResults = do
  load <- loadPreviousResults
  if load then do
    contents <- B.readFile jsonfile
    let savedResults = decode contents :: Maybe MetaResult
    case savedResults of
      Just s -> return s
      Nothing -> return (MetaResult [])
  else 
    return (MetaResult [])

-- helper function that asks user if they want to load from previous session or start new
loadPreviousResults :: IO Bool
loadPreviousResults = do
  putStrLn "Do you want to load results from previous session?(y/n)"
  response <- getLine
  case response of
    "y" -> return True
    "n" -> return False
    _ -> do
      putStrLn "Invalid response. Please enter 'y' or 'n'."
      loadPreviousResults      

-- function that saves the MetaResults data to JSON file 
saveMetaResults :: MetaResult -> IO ()
saveMetaResults updatedResults = do
  B.writeFile jsonfile (encode updatedResults)
  
-- function that displays all the past games results at the end of session 
displayResults :: MetaResult -> IO ()
displayResults (MetaResult results) = do
  putStrLn "\nPrinting the results in order from most recent to most old:"
  putStrLn (printResults results)


-- helper function that returns String of all past game results
printResults :: [(String, Bool)] -> String
printResults [] = ""
printResults ((word, res):t) =  "* The word to guess was " ++ word ++ ", and you " ++ (if res then "won" else "lost") ++ " the game \n" ++ (printResults t) 



--------- Game Helper Functions ---------

-- define the difficultyLevels function
difficultyLevels :: IO [DifficultyLevel]
difficultyLevels = do
  (word, hint) <-  randomWordAndHint
  return [ DifficultyLevel "Easy" "" (\word -> unsafePerformIO (mapToRandomIndices (ceiling (fromIntegral (length word) / fromIntegral 4)) (const '_') word)),
    DifficultyLevel "Medium" "" (\word -> unsafePerformIO (mapToRandomIndices (ceiling (fromIntegral (length word) / fromIntegral 2)) (const '_') word)),
    DifficultyLevel "Hard" "" (\word -> unsafePerformIO (mapToRandomIndices (ceiling (fromIntegral (length word))) (const '_') word)),
    DifficultyLevel "English as a Second Language (ESL)" hint (\word -> unsafePerformIO (mapToRandomIndices (ceiling (fromIntegral (length word))) (const '_') word))
        ]


-- function to add a won game to MetaResults
addWonGame :: String -> MetaResult -> MetaResult
addWonGame word (MetaResult results) = (MetaResult ((word, True):results))


-- function to add a lost game to MetaResults
addLostGame :: String -> MetaResult -> MetaResult
addLostGame word (MetaResult results) = (MetaResult ((word, False):results))


-- map the function f to a certain number of characters n in the word xs with indices picked at random
mapToRandomIndices :: Int -> (a -> a) -> [a] -> IO [a]
mapToRandomIndices n f xs = do
  let len = length xs
  indices <- replicateM n $ randomRIO (0, len - 1)
  return $ map (\(i, x) -> if i `elem` indices then f x else x) $ zip [0..] xs


-- function that checks if a guess if correct or not
isCorrect :: String -> String -> Bool
isCorrect guess word = guess == map toLower word


-- function that checks if the guess is valid
isValidGuess :: String -> Bool
isValidGuess (h:t) = null t && isLower h
isValidGuess "" = False


-- function that shows the hint of the word if it exists for ESL mode
showHint :: String -> String
showHint hint =
  if hint == ""
    then ""
    else "Hint: " ++ hint ++ "\n"


-- function that checks the guess against the word and update 'guessed' accordingly 
checkGuess :: String -> Char -> MapIndices -> MapIndices
checkGuess word guess guessed =  
  [if c == guess && g == '_' then c else g | (c, g) <- zip word guessed]
  

-- function that gets a random word and its corresponding hint from the txt files
randomWordAndHint :: IO WordAndHint
randomWordAndHint = do
  wordlist <- readFile wordfile
  hintlist <- readFile hintfile
  let words = lines wordlist
      hints = lines hintlist
  index <- randomRIO (0, length words - 1)
  let word = words !! index
      hint = hints !! index
  return (word, hint)