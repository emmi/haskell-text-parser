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

getLocationBefore :: [Location] -> String -> String
getLocationBefore [] _ = "Don't know."
getLocationBefore [x] _ = "Don't know."
getLocationBefore (firstLocation:others) location
  | locationName firstLocation == location = "Don't know."
  | locationName (head others) == location = locationName firstLocation
  | otherwise = getLocationBefore others location


getLocationAfter :: [Location] -> String -> String
getLocationAfter [] _ = "Don't know."
getLocationAfter [x] _ = "Don't know."
getLocationAfter (firstLocation:others) location
  | locationName firstLocation == location = locationName (head others)
  | otherwise = getLocationAfter others location
