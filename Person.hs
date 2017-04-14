module Person where

import Data.List
import Location
import Item

data Person = Person {
  personName :: String,
  objects :: Int,
  locations :: [Location]
} deriving (Show, Eq)

findPerson :: [Person] -> String -> Maybe Person
findPerson people name = find (\person -> (personName person) == name) people


updatePersonLocation :: [Person] -> String -> String -> [Person]
updatePersonLocation (person:people) newName newLocation
  | (personName person) == newName = person { locations = (locations person) ++ [Location newLocation]} : people
  | otherwise = [person] ++ updatePersonLocation people newName newLocation


updatePersonItems :: [Person] -> String -> Bool -> [Person]
updatePersonItems (person:people) newName takeItem
 | (personName person) == newName = if (takeItem)
                                    then person { objects = (objects person) + 1} : people
                                    else person { objects = (objects person) - 1} : people
 | otherwise = [person] ++ updatePersonItems people newName takeItem


getLocation :: [Person] -> String -> Location
getLocation people owner =
 case findPerson people owner of
   Just value -> last (locations value)
   Nothing -> error "location not found"
