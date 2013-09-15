module AI.Action where

import Support.Coords

-- Commands which an actor AI may issue in response to Tick
data Action = Attack -- Damage another entity
            | Eat -- Consume an entity (dead or living!)
            | Go Cardinal -- Actor goes to a different location
            | Take | Put -- Move stuff between world and inventory
            | Open | Close -- Door, chest, portal...?
            | Ask | Tell -- Includes reading and writing
            | Copulate -- Laying eggs???
            | Rest -- Remain still and recuperate
            | DoNothing -- Really do nothing
            | Look -- NOTE: Looking around doesn't consume a turn!
  deriving (Eq, Show)
