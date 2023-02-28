{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

import GHC.Generics (Generic)
import Data.Aeson (FromJSON, ToJSON, encode, decode)
import qualified Data.ByteString.Lazy as B

data Person = Person { name :: String, age :: Int } deriving (Show, Eq, Generic)

-- Define JSON encoding and decoding for Person type
instance FromJSON Person
instance ToJSON Person

-- Write a list of Persons to a JSON file
writePersons :: FilePath -> [Person] -> IO ()
writePersons path persons = B.writeFile path (encode persons)

-- Read a list of Persons from a JSON file
readPersons :: FilePath -> IO (Maybe [Person])
readPersons path = do
  contents <- B.readFile path
  return $ decode contents

-- Example usage
main :: IO ()
main = do
  let persons = [Person "Alice" 25, Person "Bob" 30]
  writePersons "persons.json" persons

  -- Read the persons from the file and print them
  maybePersons <- readPersons "persons.json"
  case maybePersons of
    Just persons -> print persons
    Nothing -> putStrLn "Failed to read persons from file."