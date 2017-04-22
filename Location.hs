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
