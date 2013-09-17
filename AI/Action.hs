module AI.Action (Action(..)) where

import Support.Coords

type EID = Int

-- Commands which an actor AI may issue in response to Tick
data Action = Attack -- Damage another entity
            | Eat !EID -- Consume an entity (dead or living!)
            | Go !Cardinal -- Actor goes to a different location

            | Pickup !EID -- Move object from world to inventory
            | Drop !EID -- Put inventory object in world
            | Open !EID -- Open door/container
            | Close !EID -- Close door/container

            | Ask -- Includes reading
            | Tell -- Includes writing
            | Rest -- Remain still and recuperate
            | DoNothing -- Really do nothing

            -- NOTE: Looking around doesn't consume a turn!
            | Look -- Examine world
            | Inspect !EID -- Examine an entity
  deriving (Eq, Show)
