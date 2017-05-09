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
  items :: [Item],
  directions :: [Direction]
} deriving (Show)

handleMove :: State -> String -> String -> Bool -> State
handleMove state newName newLocation isUncertain =
  let allPeople = people state
      updatedItems = moveItems (items state) newName newLocation
  in case findPerson allPeople newName of
    Just value -> state { people = updatePersonLocation allPeople newName newLocation isUncertain, items = updatedItems }
    Nothing -> state { people = people state ++ [Person newName 0 [Location newLocation] isUncertain] }

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
    Nothing -> state { people = people state ++ [Person newName 1 [] False] , items = updatedItems }

handleOwnerChange :: State -> String -> String -> String -> State
handleOwnerChange state prevOwner newOwner itemName =
  handleTakeAndGive (handleTakeAndGive state prevOwner False itemName) newOwner True itemName

handleUncertainMove :: State -> String -> String -> String -> State
handleUncertainMove state personName location1 location2 =
  handleMove (handleMove state personName location1 True) personName location2 True

saveDirection :: State -> String -> EDirection -> String -> State
saveDirection state from direction to =
  state { directions = directions state ++ [Direction from direction to]}

updateState :: Command -> State -> State
updateState command state =
  case command of
    Move (EPerson (Ident personName)) (ELocation (Ident locationName)) -> do
      handleMove state personName locationName False
    NoMore (EPerson (Ident personName)) (ELocation (Ident locationName)) -> do
      handleMove state personName "" True
    Either (EPerson (Ident personName)) (ELocation (Ident location1)) (ELocation (Ident location2)) -> do
      handleUncertainMove state personName location1 location2
    Take (EPerson (Ident personName)) (EItem (Ident itemName)) -> do
      handleTakeAndGive state personName True itemName
    Handed (EPerson (Ident prevOwner)) (EItem (Ident itemName)) (EPerson (Ident newOwner)) -> do
      handleOwnerChange state prevOwner newOwner itemName
    Give (EPerson (Ident personName)) (EItem (Ident itemName)) -> do
      handleTakeAndGive state personName False itemName
    DirectionTo (ELocation (Ident from)) direction (ELocation (Ident to)) ->
      saveDirection state from direction to

loop state = do
  line <- getLine
  unless (null line) $
    let Ok e = pCommand (myLexer line) in
      case e of
        IsIn (EPerson (Ident personName)) (ELocation (Ident locationName)) -> do
          putStrLn $ isPersonIn (people state) personName locationName
          loop state
        WhereIs (EItem (Ident itemName)) -> do
          putStrLn $ getItemLocation (items state) itemName
          loop state
        HowMany (EPerson (Ident personName)) -> do
          putStrLn $ getObjectCount (people state) personName
          loop state
        WhereWasBefore (EPerson (Ident personName)) (ELocation (Ident locationName)) -> do
          putStrLn $ whereWas (people state) personName locationName True
          loop state
        WhereWasAfter (EPerson (Ident personName)) (ELocation (Ident locationName)) -> do
          putStrLn $ whereWas (people state) personName locationName False
          loop state
        HowDo (ELocation (Ident from)) (ELocation (Ident to)) -> do
          putStrLn $ getDirections (directions state) from to
          loop state
        _ -> do
          let updatedState = updateState e state
          loop updatedState

main = loop (State [] [] [])
