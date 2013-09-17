module Support.Measure where

rdiv :: Int -> Int -> Int
rdiv q d = round $ toRational q / toRational d

lerp xj (xi, yi) (xk, yk)
  | xj < xi = yi
  | xj > xk = yk
  | otherwise = yi + div ((yk - yi) * (xj - xi)) (xk - xi)
