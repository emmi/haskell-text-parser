module Location where

import Data.List
import Data.Maybe
import AbsText


data Location = Location {
  locationName :: String
} deriving (Show, Eq)

data Direction = Direction {
  from :: String,
  direction :: EDirection,
  to :: String
} deriving (Show, Eq)

hasEntry location (from, direction, to) = from == location || to == location

isSame :: Location -> String -> Bool
isSame location target = locationName location == target

checkLocation :: Location -> String -> String
checkLocation lastLocation location
  | isSame lastLocation location = "Yes"
  | otherwise = "No"

isLocationPossible :: [Location] -> String -> String
isLocationPossible (first:second:others) location
  | isSame first "" = if (isSame second location)
                                        then "No"
                                        else "Maybe"
  | isSame first location || isSame second location = "Maybe"
  | otherwise = "No"

getLocationBefore :: [Location] -> String -> String
getLocationBefore [] _ = "Don't know."
getLocationBefore [x] _ = "Don't know."
getLocationBefore (first:others) location
  | isSame first location = "Don't know."
  | isSame (head others) location = locationName first
  | otherwise = getLocationBefore others location

getLocationAfter :: [Location] -> String -> String
getLocationAfter [] _ = "Don't know."
getLocationAfter [x] _ = "Don't know."
getLocationAfter (first:others) location
  | isSame first location = locationName (head others)
  | otherwise = getLocationAfter others location

getDirections :: [Direction] -> String -> String -> String
getDirections allPaths from to =
  case howToGet allPaths from to of
    Just path -> concat $ intersperse ", " (map getDirectionName path)
    Nothing -> "Don't know"

getDirectionName :: EDirection -> String
getDirectionName EWest = "west"
getDirectionName EEast = "east"
getDirectionName ENorth = "north"
getDirectionName ESouth = "south"

howToGet :: [Direction] -> String -> String -> Maybe [EDirection]
howToGet [] _ _ = Nothing
howToGet allPaths f t =
  let targetPaths = filter (\path -> from path == f || to path == f ) allPaths
      otherPaths = allPaths \\ targetPaths
      checkPath :: Direction -> Maybe [EDirection]
      checkPath path =
        let
          direction_ = if from path == f then flipDirection (direction path) else direction path
          nextStart = if from path == f then to path else from path
        in
          if from path == t || to path == t then
            Just [direction_]
          else
            (++) [direction_] `fmap` howToGet otherPaths nextStart t
  in
    if null targetPaths
    then Nothing
    else
      let successes = filter isJust (map checkPath targetPaths)
      in
        if null successes
        then Nothing
        else head successes

flipDirection :: EDirection -> EDirection
flipDirection ESouth = ENorth
flipDirection ENorth = ESouth
flipDirection EWest = EEast
flipDirection EEast = EWest
