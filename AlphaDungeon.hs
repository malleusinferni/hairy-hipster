module AlphaDungeon where

import Support.Coords
import GameTypes
import Entity.Material

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
          , Room
            { onGrid = zyx 0 1 0
            , roomName = "grotto"
            , description = "Luminescent fungus dangles from the ceiling."
            , walls = Stone
            , floors = Stone
            }
          , Room
            { onGrid = zyx 0 0 (-1)
            , roomName = "library"
            , description = "Leathery tomes are stacked against the walls."
            , walls = Masonry
            , floors = Masonry
            }
          , Room
            { onGrid = zyx 0 1 (-1)
            , roomName = "sepulchre"
            , description = "Torches illuminate the entrance to the tomb."
            , walls = Sandstone
            , floors = Sand
            }
          , Room
            { onGrid = zyx 0 1 (-2)
            , roomName = "tomb"
            , description = "The stone casket has been opened recently."
            , walls = Dirt
            , floors = Dirt
            }
          , Room
            { onGrid = zyx 0 (-1) (-2)
            , roomName = "pit"
            , description = "A yawning chasm opens to the west."
            , walls = Stone
            , floors = Stone
            }
          , Room 
            { onGrid = zyx 0 (-2) (-1)
            , roomName = "pantry"
            , description = "The shelves are stuffed with stores of food."
            , walls = Wood
            , floors = Masonry
            }
          , Room
            { onGrid = zyx 0 (-3) (-2)
            , roomName = "chapel"
            , description = "It is very bright here."
            , walls = Wood
            , floors = Wood
            }
          ]
        corridors =
          [ Corridor (zyx 1 0 0, zyx 0 0 0) "well"
          , Corridor (zyx 0 0 0, zyx 0 (-1) 0) "secret door"
          , Corridor (zyx 0 (-1) 0, zyx 0 (-1) 1) "tunnel"
          , Corridor (zyx 0 (-1) 1, zyx 0 (-2) 1) "bridge"
          , Corridor (zyx 0 (-1) 1, zyx 0 1 0) "crevice"
          , Corridor (zyx 0 1 0, zyx 0 0 (-1)) "stone door"
          , Corridor (zyx 0 0 (-1), zyx 0 1 (-1)) "archway"
          , Corridor (zyx 0 1 (-1), zyx 0 1 (-2)) "slab door"
          , Corridor (zyx 0 0 (-1), zyx 0 (-1) (-2)) "ledge"
          , Corridor (zyx 0 0 (-1), zyx 0 (-2) (-1)) "wooden door"
          , Corridor (zyx 0 (-1) (-2), zyx 0 (-3) (-2)) "steps"
          , Corridor (zyx 0 (-2) (-1), zyx 0 (-3) (-2)) "hall"
          ]
