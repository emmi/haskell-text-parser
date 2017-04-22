module Location where

import Data.List

data Location = Location {
  locationName :: String
} deriving (Show, Eq)


checkLocation :: Location -> String -> String
checkLocation lastLocation location
  | locationName lastLocation == location = "Yes"
  | otherwise = "No"


isLocationPossible :: [Location] -> String -> String
isLocationPossible (firstLocation:secondLocation:others) location
  | locationName firstLocation == location || locationName secondLocation == location = "Maybe"
  | otherwise = "No"

getLocationNow :: [Location] -> String -> String
getLocationNow [] _ = "Don't know."
getLocationNow [x] _ = "Don't know."
getLocationNow (firstLocation:others) location
  | locationName firstLocation == location = "Don't know."
  | locationName (head others) == location = locationName firstLocation
  | otherwise = getLocationNow others location
