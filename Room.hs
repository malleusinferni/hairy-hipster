module Room where

data Room = Room {
    placename :: String,
    doors :: [(Door, Room)]
  }

data Door = Door {
    doortype :: String,
    doordir :: Cardinal
  }

data Cardinal = North | East | South | West | Up | Down
