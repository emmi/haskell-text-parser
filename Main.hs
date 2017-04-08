module Main where

import Control.Monad

import LexText
import ParText
import AbsText
import Data.List

import ErrM

data State = State {
  people :: [Person]
} deriving (Show)

data Person = Person {
  name :: String,
  locations :: [Location]
} deriving (Show, Eq)

data Location = Location {
  locationName :: String
} deriving (Show, Eq)


updatePersonLocation :: [Person] -> String -> String -> [Person]
updatePersonLocation (person:people) newName newLocation
  | (name person) == newName = person { locations = (locations person) ++ [Location newLocation]} : people
  | otherwise = [person] ++ updatePersonLocation people newName newLocation

alreadyExists :: [Person] -> String -> Maybe Person
alreadyExists people newName = find (\person -> (name person) == newName) people

handleMove :: State -> String -> String -> State
handleMove state newName newLocation =
  let allPeople = people state
  in case alreadyExists allPeople newName of
    Just value -> state { people = updatePersonLocation allPeople newName newLocation }
    Nothing -> state { people = people state ++ [Person newName 0 [Location newLocation]] }

loop state = do
  line <- getLine
  unless (null line) $
    let Ok e = pCommand (myLexer line) in
      case e of
        Move (EPerson (Ident personName)) (ELocation (Ident locationName)) -> do
          let newState = handleMove state personName locationName
          mapM_ print (people newState)
          loop newState
          loop state

main = loop (State [])
