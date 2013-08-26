{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE Rank2Types #-}

module Describe where

import Data.Char (toUpper, toLower)

data Verb = Verb {
    infinitive :: String,
    presentSingular :: String,
    presentPlural :: String,
    pastSingular :: String,
    pastPlural :: String,
    gerund :: String,
    danglingPreposition :: Maybe String
  } deriving (Eq, Show)

data Noun = Noun {
    nominative :: String,
    genitive :: String,
    accusative :: String,
    plural :: Bool
  } deriving (Eq, Show)

data Dependent = I | You | He | She | It | They
               | forall n. Nominable n => The n
               | forall n. Nominable n => An n

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
  where reg nom gen = Noun nom gen nom
        nmap f w = let (Noun n g a p) = name w in
                       Noun (f n) (f g) (f a) p
        the w = "the " ++ w
        an w@(c:_)
          | isVowel c = "an " ++ w
          | otherwise = "a " ++ w
        isVowel c = elem c "aeiouAEIOU"

conj n
  | plural (name n) = presentPlural
  | otherwise = presentSingular

sentence :: Effable d => d -> String -> String
sentence d end = upcase (describe d) ++ end

unsentence :: Effable d => [d] -> String
unsentence = unlines . map (`sentence` ".")

upcase (c:cs) = toUpper c : cs
downcase (c:cs) = toLower c : cs

class Nominable a where
  name :: a -> Noun

instance Nominable Noun where
  name = id

class Effable a where
  describe :: a -> String
