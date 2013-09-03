{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE Rank2Types #-}

module Describe where

import Data.Char (toUpper, toLower)
import Data.List (intercalate)

data Verb = Verb
  { infinitive :: String
  , presentSingular :: String
  , presentPlural :: String
  , pastSingular :: String
  , pastPlural :: String
  , gerund :: String
  , danglingPreposition :: Maybe String
  } deriving (Eq, Show)

data Noun = Noun
  { nominative :: String
  , genitive :: String
  , accusative :: String
  , plural :: Bool
  } deriving (Eq, Show)

data Dependent = I | You | He | She | It | They
               | forall n. Nominable n => The n
               | forall n. Nominable n => An n
               | forall n. Effable n => Adj String n

verb :: String -> Verb
verb v =
  case v of
    "be" -> Verb "be" "is" "are" "was" "were" "being" Nothing
    "hit" -> reg "hit hits hit hitting"
    "collapse" -> reg "collapse collapses collapsed collapsing"
    "perish" -> reg "perish perishes perished perishing"
    "wound" -> reg "wound wounds wounded wounding"
    "emerge" -> reg "emerge emerges emerged emerging"
    "lurk" -> reg "lurk lurks lurked lurking"
    "die" -> reg "die dies died dying"
    _ -> reg $ defaultVerb v
  where reg text =
         case words text of
          [inf, sing, past, ger] -> Verb inf sing inf past past ger Nothing
          _ -> error ("WRONG VERB FORMAT: " ++ text)
        defaultVerb v = unwords [v, v ++ "s", v ++ "d", v ++ "ing"]

noun :: Dependent -> Noun
noun n =
  case n of
    I -> Noun "I" "my" "me" False
    He -> Noun "he" "his" "him" False
    She -> Noun "she" "her" "her" False
    They -> Noun "they" "their" "them" True

    You -> reg "you" "your" True
    It -> reg "it" "its" False
    The w -> nmap the w
    An w -> nmap an w
    Adj a w -> let n = unwords [a, describe w] in reg n (n ++ "'s") False
  where reg nom gen = Noun nom gen nom
        nmap f w = let (Noun n g a p) = name w in
                       Noun (f n) (f g) (f a) p
        the w = "the " ++ w
        an w@(c:_)
          | isVowel c = "an " ++ w
          | otherwise = "a " ++ w
        an [] = "something"
        isVowel c = c `elem` "aeiouAEIOU"

conj :: Nominable n => n -> Verb -> String
conj n
  | plural (name n) = presentPlural
  | otherwise = presentSingular

sentence :: Effable d => d -> String -> String
sentence d end = upcase (describe d) ++ end

unsentence :: Effable d => [d] -> String
unsentence = intercalate "\n" . map (`sentence` ".")

upcase, downcase :: String -> String
upcase (c:cs) = toUpper c : cs
upcase [] = []

downcase (c:cs) = toLower c : cs
downcase [] = []

subj, obj, poss :: (Nominable a) => a -> String
subj = nominative . name
obj = accusative . name
poss = genitive . name

cverb :: (Nominable a) => a -> String -> String
cverb a = conj a . verb

aeverb :: (Nominable a, Show b) => a -> b -> String
aeverb a = cverb a . downcase . show

numWord :: Int -> String
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

class Nominable a where
  name :: a -> Noun

instance Nominable Noun where
  name = id

instance Nominable Dependent where
  name = noun

class Effable a where
  describe :: a -> String
