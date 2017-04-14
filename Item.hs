module Item where

import Data.List
import Location

data Item = Item {
  itemName :: String,
  owner :: String,
  location :: Location
} deriving (Show, Eq)


findItem :: [Item] -> String -> Maybe Item
findItem items name = find (\item -> (itemName item) == name) items

moveItems :: [Item] -> String -> String -> [Item]
moveItems (item:items) person newLocation = if (owner item) == person then [item {location = Location newLocation}] ++ moveItems items person newLocation
                                            else [item] ++ moveItems items person newLocation
moveItems [] _ _ = []


getItemLocation :: [Item] -> String -> Location
getItemLocation items itemName =
  case findItem items itemName of
    Just item -> location item
    Nothing -> error "location not found"
