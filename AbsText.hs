

module AbsText where

-- Haskell module generated by the BNF converter




newtype Ident = Ident String deriving (Eq, Ord, Show, Read)
data Command
    = IsIn EPerson ELocation
    | WhereIs EItem
    | HowMany EPerson
    | WhereWasBefore EPerson ELocation
    | WhereWasAfter EPerson ELocation
    | HowDo ELocation ELocation
    | Either EPerson ELocation ELocation
    | NoMore EPerson ELocation
    | Move EPerson ELocation
    | Take EPerson EItem
    | Handed EPerson EItem EPerson
    | Give EPerson EItem
    | DirectionTo ELocation EDirection ELocation
  deriving (Eq, Ord, Show, Read)

data ELocation = ELocation Ident
  deriving (Eq, Ord, Show, Read)

data EDirection = EWest | EEast | ENorth | ESouth
  deriving (Eq, Ord, Show, Read)

data EItem = EItem Ident
  deriving (Eq, Ord, Show, Read)

data EPerson = EPerson Ident
  deriving (Eq, Ord, Show, Read)

