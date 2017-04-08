module Main where

import Control.Monad

import LexText
import ParText
import AbsText
import Data.List

import ErrM

data State = State {
  people :: [Person]
} deriving (Show)

data Person = Person {
  name :: String
} deriving (Show, Eq)

loop state = do
  line <- getLine
  unless (null line) $
    let Ok e = pCommand (myLexer line) in
      case e of
        Move (EPerson (Ident personName)) (ELocation (Ident locationName)) -> do
          loop state

main = loop (State [])
