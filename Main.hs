module Main where

import Control.Monad

import LexText
import ParText
import AbsText
import Data.List

import ErrM

data State = State {
  people :: [Person],
  items :: [Item]
} deriving (Show)

data Person = Person {
  name :: String,
  objects :: Int,
  locations :: [Location]
} deriving (Show, Eq)

data Location = Location {
  locationName :: String
} deriving (Show, Eq)

data Item = Item {
  itemName :: String,
  owner :: String,
  location :: Location
} deriving (Show, Eq)

updatePersonLocation :: [Person] -> String -> String -> [Person]
updatePersonLocation (person:people) newName newLocation
  | (name person) == newName = person { locations = (locations person) ++ [Location newLocation]} : people
  | otherwise = [person] ++ updatePersonLocation people newName newLocation

updatePersonItems :: [Person] -> String -> Int -> [Person]
updatePersonItems (person:people) newName value
 | (name person) == newName = person { objects = (objects person) + value} : people
 | otherwise = [person] ++ updatePersonItems people newName value

findPerson :: [Person] -> String -> Maybe Person
findPerson people newName = find (\person -> (name person) == newName) people

findItem :: [Item] -> String -> Maybe Item
findItem items newItem = find (\item -> (itemName item) == newItem) items

getLocation :: [Person] -> String -> Location
getLocation people owner =
  case findPerson people owner of
    Just value -> last (locations value)
    Nothing -> error "location not found"

handleMove :: State -> String -> String -> State
handleMove state newName newLocation =
  let allPeople = people state
  in case findPerson allPeople newName of
    Just value -> state { people = updatePersonLocation allPeople newName newLocation }
    Nothing -> state { people = people state ++ [Person newName 0 [Location newLocation]] }

updateItemState :: State -> String -> Int -> String -> [Item]
updateItemState state owner value item =
  let currentLocation = getLocation (people state) owner
  in case findItem (items state) item of
    Just value -> (items state)
    Nothing -> (items state) ++ [Item item owner currentLocation]

handleTakeAndGive :: State -> String -> Int -> String -> State
handleTakeAndGive state newName value item =
  let allPeople = people state
      allItems = items state
      updatedItems = updateItemState state newName value item
  in case findPerson allPeople newName of
    Just test -> state { people = updatePersonItems allPeople newName value, items = updatedItems }
    Nothing -> state { people = people state ++ [Person newName value []], items = updatedItems }

isPersonIn :: State -> String -> String -> String
isPersonIn state name location =
  let allPeople = people state
  in case findPerson allPeople name of
    Just value -> checkLocation (last (locations value)) location
    Nothing -> "Maybe"

checkLocation :: Location -> String -> String
checkLocation lastLocation location
  | locationName lastLocation == location = "Yes"
  | otherwise = "No"


loop state = do
  line <- getLine
  unless (null line) $
    let Ok e = pCommand (myLexer line) in
      case e of
        Move (EPerson (Ident personName)) (ELocation (Ident locationName)) -> do
          let newState = handleMove state personName locationName
          mapM_ print (people newState)
          mapM_ print (items newState)
          loop newState
        Take (EPerson (Ident personName)) (EItem (Ident itemName)) -> do
          let value = 1
          let newState = handleTakeAndGive state personName value itemName
          mapM_ print (people newState)
          mapM_ print (items newState)
          loop newState
        Give (EPerson (Ident personName)) (EItem (Ident itemName)) -> do
          let value = -1
          let newState = handleTakeAndGive state personName value itemName
          mapM_ print (people newState)
          mapM_ print (items newState)
          loop newState
        IsIn (EPerson (Ident personName)) (ELocation (Ident locationName)) -> do
          putStrLn $ isPersonIn state personName locationName
          loop state


main = loop (State [] [])
