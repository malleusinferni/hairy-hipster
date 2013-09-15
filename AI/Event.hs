module AI.Event where

-- Effect of the action on the patient (IMPORTANT!)
data Outcome = NothingHappens
             | TakeDamage
             | NearDeath
             | Heal
             | Stand
             | Walk
             | See
             | Die
             | Win
             | Lose
             | Fail
  deriving (Eq, Show)
