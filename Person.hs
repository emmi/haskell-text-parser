module Person where

import Data.List
import Location
import Item

data Person = Person {
  personName :: String,
  objects :: Int,
  locations :: [Location],
  locationIsUncertain :: Bool
} deriving (Show, Eq)

findPerson :: [Person] -> String -> Maybe Person
findPerson people name = find (\person -> (personName person) == name) people


updatePersonLocation :: [Person] -> String -> String -> Bool -> [Person]
updatePersonLocation (person:people) newName newLocation isUncertain
  | (personName person) == newName = person { locations = (locations person) ++ [Location newLocation], locationIsUncertain = isUncertain } : people
  | otherwise = [person] ++ updatePersonLocation people newName newLocation isUncertain


updatePersonItems :: [Person] -> String -> Bool -> [Person]
updatePersonItems (person:people) newName takeItem
 | (personName person) == newName = if (takeItem)
                                    then person { objects = (objects person) + 1} : people
                                    else person { objects = (objects person) - 1} : people
 | otherwise = [person] ++ updatePersonItems people newName takeItem


getLocation :: [Person] -> String -> Location
getLocation people owner =
 case findPerson people owner of
   Just value -> if (null (locations value))
                 then Location ""
                 else last (locations value)
   Nothing -> Location ""

isPersonIn :: [Person] -> String -> String -> String
isPersonIn people name location =
  case findPerson people name of
   Just person -> if locationIsUncertain person
                 then isLocationPossible (reverse (locations person)) location
                 else checkLocation (last (locations person)) location
   Nothing -> "Maybe"

getObjectCount :: [Person] -> String -> String
getObjectCount people owner =
  case findPerson people owner of
    Just person -> show (objects person)
    Nothing -> "Don't know."

whereWas :: [Person] -> String -> String -> Bool -> String
whereWas people name location wasBefore =
  case findPerson people name of
    Just person -> if wasBefore
                  then getLocationBefore (locations person) location
                  else getLocationAfter (locations person) location
    Nothing -> "Don't know."
