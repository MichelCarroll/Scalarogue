package dungeon.generation

sealed trait Tile
case object RoomTile extends Tile
case object CorridorTile extends Tile
case object DoorTile extends Tile

