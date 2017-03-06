

sealed trait Tile {

  def isRoomTile: Boolean = this match {
    case RoomTile => true
    case CorridorTile => false
    case DoorTile => false
  }

  def isCorridorTile: Boolean = this match {
    case RoomTile => false
    case CorridorTile => true
    case DoorTile => false
  }

}
case object RoomTile extends Tile
case object CorridorTile extends Tile
case object DoorTile extends Tile

