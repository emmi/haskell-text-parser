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

updateItemState :: State -> String -> Bool -> String -> [Item]
updateItemState state owner takeItem item =
  let currentLocation = getLocation (people state) owner
  in case findItem (items state) item of
    Just value -> if (takeItem)
                  then (items state)
                  else dropItem (items state) item
    Nothing -> (items state) ++ [Item item owner currentLocation]

handleTakeAndGive :: State -> String -> Bool -> String -> State
handleTakeAndGive state newName takeItem item =
  let allPeople = people state
      allItems = items state
      value = 1
      updatedItems = updateItemState state newName takeItem item
  in case findPerson allPeople newName of
    Just test -> state { people = updatePersonItems allPeople newName takeItem, items = updatedItems }
    Nothing -> state { people = people state ++ [Person newName 1 []], items = updatedItems }

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
      handleMove state personName locationName
    Take (EPerson (Ident personName)) (EItem (Ident itemName)) -> do
      handleTakeAndGive state personName True itemName
    Give (EPerson (Ident personName)) (EItem (Ident itemName)) -> do
      handleTakeAndGive state personName False itemName

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
