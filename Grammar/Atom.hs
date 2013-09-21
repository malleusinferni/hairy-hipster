{-# LANGUAGE BangPatterns, OverloadedStrings, FlexibleInstances #-}
module Grammar.Atom
  ( Atom
  , Leaf(..)
  , sentence
  , unleaves
  , capital
  , commas
  ) where

import Data.Char (toUpper)
import Data.Text (Text(..))
import qualified Data.Text as T

import GHC.Exts (IsString(..))

type Atom = Text

data Leaf = Begin
          | Word { text :: !Atom }
          | Punct { text :: !Atom }
  deriving (Eq, Show)

instance IsString [Leaf] where
  fromString = map wordOrPunct . words

wordOrPunct :: String -> Leaf
wordOrPunct x
  | x `elem` words ". , ... !" = Punct (T.pack x)
  | otherwise = Word (T.pack x)

sentence :: [Leaf] -> [Leaf]
sentence = (Begin :) . (++ [Punct "."])

unleaves :: [Leaf] -> Text
unleaves = T.concat . pre
  where pre (Begin : Word x : xs) = capital x : pre xs
        pre (Begin : xs) = pre xs
        pre (Punct p : xs) = p : pre xs
        pre (Word t : xs) = T.cons ' ' t : pre xs
        pre [] = []

capital :: Text -> Text
capital w =
  case T.uncons w of
    Just (c, rest) -> T.cons (toUpper c) rest
    Nothing -> w

commas :: [[Leaf]] -> [Leaf]
commas (x:xs) = concat (x : map (Punct "," :) xs)

test :: Text
test = unleaves . sentence . commas $
  ["the wyrm bites your head off", "killing you instantly"]
