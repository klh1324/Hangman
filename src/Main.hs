module Main where

import System.Random (randomRIO)

getInt :: IO Int
getInt = readLn

randomWords = ["bananas", "vancouver", "blue", "baseball", "quick", "midterms"]

getRandomWord :: IO String 
getRandomWord = do
  let n = length randomWords
  rand <- randomRIO (0, n - 1)
  return (randomWords !! rand)

getNumGuesses = do
    putStrLn "What difficulty do you want? (0-4), with 0 being easy, 4 being hardest"
    difficulty <- getInt
    return (10 - difficulty)

main :: IO()
main = do
    putStrLn "Hi there! Welcome to Hangman!"
    word <- getRandomWord
    numGuesses <- getNumGuesses
    return ()
    play word 0 numGuesses

play :: String -> Int -> Int -> IO ()
play word guessed guessesLeft 
    | guessesLeft == 0 = do 
        putStrLn "Sorry! Game over no more guesses"
        putStrLn $ "The correct word was " ++ word
    | otherwise = do 
        putStrLn "Continue playing!"
        play word guessed (guessesLeft - 1)


