{-# OPTIONS_GHC -fno-warn-incomplete-patterns #-}
module PrintText where

-- pretty-printer generated by the BNF converter

import AbsText
import Data.Char


-- the top-level printing method
printTree :: Print a => a -> String
printTree = render . prt 0

type Doc = [ShowS] -> [ShowS]

doc :: ShowS -> Doc
doc = (:)

render :: Doc -> String
render d = rend 0 (map ($ "") $ d []) "" where
  rend i ss = case ss of
    "["      :ts -> showChar '[' . rend i ts
    "("      :ts -> showChar '(' . rend i ts
    "{"      :ts -> showChar '{' . new (i+1) . rend (i+1) ts
    "}" : ";":ts -> new (i-1) . space "}" . showChar ';' . new (i-1) . rend (i-1) ts
    "}"      :ts -> new (i-1) . showChar '}' . new (i-1) . rend (i-1) ts
    ";"      :ts -> showChar ';' . new i . rend i ts
    t  : ts@(p:_) | closingOrPunctuation p -> showString t . rend i ts
    t        :ts -> space t . rend i ts
    _            -> id
  new i   = showChar '\n' . replicateS (2*i) (showChar ' ') . dropWhile isSpace
  space t = showString t . (\s -> if null s then "" else ' ':s)
  closingOrPunctuation [c] = c `elem` ")],;"
  closingOrPunctuation _   = False

parenth :: Doc -> Doc
parenth ss = doc (showChar '(') . ss . doc (showChar ')')

concatS :: [ShowS] -> ShowS
concatS = foldr (.) id

concatD :: [Doc] -> Doc
concatD = foldr (.) id

replicateS :: Int -> ShowS -> ShowS
replicateS n f = concatS (replicate n f)

-- the printer class does the job
class Print a where
  prt :: Int -> a -> Doc
  prtList :: Int -> [a] -> Doc
  prtList i = concatD . map (prt i)

instance Print a => Print [a] where
  prt = prtList

instance Print Char where
  prt _ s = doc (showChar '\'' . mkEsc '\'' s . showChar '\'')
  prtList _ s = doc (showChar '"' . concatS (map (mkEsc '"') s) . showChar '"')

mkEsc :: Char -> Char -> ShowS
mkEsc q s = case s of
  _ | s == q -> showChar '\\' . showChar s
  '\\'-> showString "\\\\"
  '\n' -> showString "\\n"
  '\t' -> showString "\\t"
  _ -> showChar s

prPrec :: Int -> Int -> Doc -> Doc
prPrec i j = if j<i then parenth else id


instance Print Integer where
  prt _ x = doc (shows x)


instance Print Double where
  prt _ x = doc (shows x)


instance Print Ident where
  prt _ (Ident i) = doc (showString ( i))



instance Print Command where
  prt i e = case e of
    IsIn eperson elocation -> prPrec i 0 (concatD [doc (showString "Is"), prt 0 eperson, prt 0 elocation, doc (showString "?")])
    WhereIs eitem -> prPrec i 0 (concatD [doc (showString "Where"), doc (showString "is"), prt 0 eitem, doc (showString "?")])
    HowMany eperson -> prPrec i 0 (concatD [doc (showString "How"), doc (showString "many"), doc (showString "objects"), doc (showString "is"), prt 0 eperson, doc (showString "carrying"), doc (showString "?")])
    WhereWasBefore eperson elocation -> prPrec i 0 (concatD [doc (showString "Where"), doc (showString "was"), prt 0 eperson, doc (showString "before"), prt 0 elocation, doc (showString "?")])
    WhereWasAfter eperson elocation -> prPrec i 0 (concatD [doc (showString "Where"), doc (showString "was"), prt 0 eperson, doc (showString "after"), prt 0 elocation, doc (showString "?")])
    HowDo elocation1 elocation2 -> prPrec i 0 (concatD [doc (showString "How"), doc (showString "do"), doc (showString "you"), doc (showString "go"), prt 0 elocation1, prt 0 elocation2, doc (showString "?")])
    Either eperson elocation1 elocation2 -> prPrec i 0 (concatD [prt 0 eperson, doc (showString "is"), doc (showString "either"), prt 0 elocation1, prt 0 elocation2])
    NoMore eperson elocation -> prPrec i 0 (concatD [prt 0 eperson, doc (showString "is"), doc (showString "no"), doc (showString "longer"), prt 0 elocation])
    Move eperson elocation -> prPrec i 0 (concatD [prt 0 eperson, doc (showString "is"), prt 0 elocation])
    Take eperson eitem -> prPrec i 0 (concatD [prt 0 eperson, doc (showString "took"), prt 0 eitem])
    Handed eperson1 eitem eperson2 -> prPrec i 0 (concatD [prt 0 eperson1, doc (showString "handed"), prt 0 eitem, doc (showString "to"), prt 0 eperson2])
    Give eperson eitem -> prPrec i 0 (concatD [prt 0 eperson, doc (showString "dropped"), prt 0 eitem])
    DirectionTo elocation1 edirection elocation2 -> prPrec i 0 (concatD [prt 0 elocation1, prt 0 edirection, prt 0 elocation2])

instance Print ELocation where
  prt i e = case e of
    ELocation id -> prPrec i 0 (concatD [doc (showString "to"), doc (showString "the"), prt 0 id])

instance Print EDirection where
  prt i e = case e of
    EWest -> prPrec i 0 (concatD [doc (showString "west"), doc (showString "of")])
    EEast -> prPrec i 0 (concatD [doc (showString "east"), doc (showString "of")])
    ENorth -> prPrec i 0 (concatD [doc (showString "north"), doc (showString "of")])
    ESouth -> prPrec i 0 (concatD [doc (showString "south"), doc (showString "of")])

instance Print EItem where
  prt i e = case e of
    EItem id -> prPrec i 0 (concatD [doc (showString "the"), prt 0 id])

instance Print EPerson where
  prt i e = case e of
    EPerson id -> prPrec i 0 (concatD [prt 0 id])


