module AlphaDungeon where

import Coords
import GameTypes

alphaDungeon :: ([Room], [Corridor])
alphaDungeon = (rooms, corridors)
  where rooms =
          [ Room
            { onGrid = zyx 1 0 0
            , roomName = "outside"
            , description = "The grass turns to ash under your feet."
            , walls = Air
            , floors = Grass
            }
          , Room
            { onGrid = zyx 0 0 0
            , roomName = "well bottom"
            , description = "Moonlight spills through the well mouth above."
            , walls = Masonry
            , floors = Stone
            }
          , Room
            { onGrid = zyx 0 (-1) 0
            , roomName = "hideout"
            , description = "A dying fire flickers on the dirt floor."
            , walls = Stone
            , floors = Dirt
            }
          , Room
            { onGrid = zyx 0 (-1) 1
            , roomName = "cavern"
            , description = "The roof of the cavern recedes into the darkness."
            , walls = Stone
            , floors = Stone
            }
          , Room
            { onGrid = zyx 0 (-2) 1
            , roomName = "lair"
            , description = "The walls are covered in blood."
            , walls = Stone
            , floors = Stone
            }
          ]
        corridors =
          [ Corridor (zyx 1 0 0, zyx 0 0 0) "well"
          , Corridor (zyx 0 0 0, zyx 0 (-1) 0) "secret door"
          , Corridor (zyx 0 (-1) 0, zyx 0 (-1) 1) "tunnel"
          , Corridor (zyx 0 (-1) 1, zyx 0 (-2) 1) "bridge"
          ]
