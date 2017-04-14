module Main where

import Control.Monad

import LexText
import ParText
import AbsText
import Data.List

import Person
import Item
import Location

import ErrM

data State = State {
  people :: [Person],
  items :: [Item]
} deriving (Show)

handleMove :: State -> String -> String -> State
handleMove state newName newLocation =
  let allPeople = people state
      updatedItems = moveItems (items state) newName newLocation
  in case findPerson allPeople newName of
    Just value -> state { people = updatePersonLocation allPeople newName newLocation, items = updatedItems }
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


updateState :: Command -> State -> State
updateState command state =
  case command of
    Move (EPerson (Ident personName)) (ELocation (Ident locationName)) -> do
      let updatedState = handleMove state personName locationName
      updatedState
    Take (EPerson (Ident personName)) (EItem (Ident itemName)) -> do
      let value = 1
      let updatedState = handleTakeAndGive state personName value itemName
      updatedState
    Give (EPerson (Ident personName)) (EItem (Ident itemName)) -> do
      let value = -1
      let updatedState = handleTakeAndGive state personName value itemName
      updatedState

loop state = do
  line <- getLine
  unless (null line) $
    let Ok e = pCommand (myLexer line) in
      case e of
        IsIn (EPerson (Ident personName)) (ELocation (Ident locationName)) -> do
          putStrLn $ isPersonIn state personName locationName
          loop state
        WhereIs (EItem (Ident itemName)) -> do
          let answer = getItemLocation (items state) itemName
          putStrLn (locationName answer)
          loop state
        _ -> do
          let updatedState = updateState e state
          mapM_ print (people updatedState)
          mapM_ print (items updatedState)
          loop updatedState

main = loop (State [] [])
