{-# LANGUAGE OverloadedStrings #-}
module Describe where

import Data.Text (Text)
import qualified Data.Text as T

import Grammar.Atom

the :: [Leaf] -> [Leaf]
the = (Word "the" :)

word = Word . T.toLower . T.pack

numWord :: Int -> Atom
numWord 1 = "one"
numWord 2 = "two"
numWord 3 = "three"
numWord 4 = "four"
numWord 5 = "five"
numWord 6 = "six"
numWord 7 = "seven"
numWord 8 = "eight"
numWord 9 = "nine"
numWord 10 = "ten"
numWord 11 = "eleven"
numWord 12 = "twelve"
numWord 13 = "thirteen"
numWord 14 = "fourteen"
numWord 15 = "fifteen"
numWord 16 = "sixteen"
numWord 17 = "seventeen"
numWord 18 = "eighteen"
numWord 19 = "nineteen"
numWord 20 = "twenty"
numWord _ = undefined

class Effable a where
  describe :: a -> [Leaf]

paragraph :: Effable d => [d] -> [Leaf]
paragraph = concat . map (sentence . describe)

fillWords :: Int -> [Leaf] -> Text
fillWords width = wrap [] . T.words . unleaves
  where wrap acc [] = T.intercalate "\n" $ reverse acc
        wrap acc@(l:ls) (w:ws) =
          -- TODO Make this less terrible
          let greedy = T.unwords [l, w] in
            if T.length greedy < width
               then wrap (greedy:ls) ws
               else wrap (w:acc) ws
        wrap acc (w:ws) = wrap (w:acc) ws
