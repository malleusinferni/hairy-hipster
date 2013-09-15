module Support.Measure where

rdiv :: Int -> Int -> Int
rdiv q d = round $ toRational q / toRational d
