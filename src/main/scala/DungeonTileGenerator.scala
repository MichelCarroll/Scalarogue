import BSPTree._
import RNG.Rand

sealed trait Tile
case object RoomTile extends Tile
case object CorridorTile extends Tile
case object DoorTile extends Tile

sealed trait Corridor
case class HorizontalCorridor(area: Area) extends Corridor
case class VerticalCorridor(area: Area) extends Corridor

case class Room(area: Area)

case class DungeonTopology(rooms: Set[Room], corridors: Set[Corridor]) {
  def +(other: DungeonTopology) = this.copy(rooms = rooms ++ other.rooms, corridors = corridors ++ other.corridors)
  def +(room: Room) = this.copy(rooms = rooms + room)
  def +(corridor: Corridor) = this.copy(corridors = corridors + corridor)
}

object DungeonTileGenerator {

  def generate(tree: BSPTree, gridSize: Size): Rand[(DungeonTopology, Map[Position, Tile])] = rng => {
    import Direction._
    var varRng = rng

    def innerTopology(tree: BSPTree, area: Area): DungeonTopology = {

      def roomTouchingCenterOfSubspace(area: Area): Rand[Room] = rng => {
        val (widthRatio, newRng1) = RNG.nextPositiveGaussianRatio(5)(rng)
        val (heightRatio, newRng2) = RNG.nextPositiveGaussianRatio(5)(newRng1)

        val roomSize = Size(
          Math.max(1, (area.size.width * (1 - widthRatio)).round.toInt),
          Math.max(1, (area.size.height * (1 - heightRatio)).round.toInt)
        )

        val minX = Math.max(area.center.x - roomSize.width + 1, area.minX)
        val minY = Math.max(area.center.y - roomSize.height + 1, area.minY)
        val maxX = Math.min(area.center.x, area.maxX - roomSize.width + 1)
        val maxY = Math.min(area.center.y, area.maxY - roomSize.height + 1)

        val (roomXPos, newRng3) = RNG.map(RNG.nextPositiveInt(maxX - minX))(_ + minX)(newRng2)
        val (roomYPos, newRng4) = RNG.map(RNG.nextPositiveInt(maxY - minY))(_ + minY)(newRng3)
        (Room(Area(Position(roomXPos, roomYPos), roomSize)), newRng4)
      }

      tree match {

        case HorizontalBranch(ratio, left, right) =>
          val (leftSize, rightSize) = area.size.partitionHorizontally(ratio)
          val leftArea = Area(area.position, leftSize)
          val rightArea = Area(area.position.right(leftSize.width), rightSize)
          innerTopology(left, leftArea) +
            innerTopology(right, rightArea) +
            HorizontalCorridor(Area(leftArea.center, rightArea.center))

        case VerticalBranch(ratio, top, bottom) =>
          val (topSize, bottomSize) = area.size.partitionVertically(ratio)
          val topArea = Area(area.position, topSize)
          val bottomArea = Area(area.position.down(topSize.height), bottomSize)
          innerTopology(top, topArea) +
            innerTopology(bottom, bottomArea) +
            VerticalCorridor(Area(topArea.center, bottomArea.center))

        case Leaf =>
          val (newRoom, newRng) = roomTouchingCenterOfSubspace(area)(varRng)
          varRng = newRng
          DungeonTopology(rooms = Set(newRoom), corridors = Set())
      }


    }

    val topology = innerTopology(tree, Area(Position(0,0), gridSize))

    val horizontallyRoomAdjacentPositions = topology.rooms
      .flatMap(room => Set(Left, Right).map(room.area.adjacencyLine))
      .flatMap(_.positions)

    val verticallyRoomAdjacentPositions = topology.rooms
      .flatMap(room => Set(Up, Down).map(room.area.adjacencyLine))
      .flatMap(_.positions)


    def isRoomTile: Tile => Boolean = {
      case RoomTile => true
      case CorridorTile => false
      case DoorTile => false
    }

    def isCorridorTile: Tile => Boolean = {
      case RoomTile => false
      case CorridorTile => true
      case DoorTile => false
    }

    val tilesWithoutDoors: Map[Position, Tile] =
        topology.corridors.flatMap {
          case VerticalCorridor(area)   =>
            area.positions.map(position =>
              if(horizontallyRoomAdjacentPositions.contains(position))
                position -> RoomTile
              else
                position -> CorridorTile
            )

          case HorizontalCorridor(area) =>
            area.positions.map(position =>
              if(verticallyRoomAdjacentPositions.contains(position))
                position -> RoomTile
              else
                position -> CorridorTile
            )
        }.toMap ++ topology.rooms.flatMap(_.area.positions.map(_ -> RoomTile)).toMap


    val tiles: Map[Position, Tile] = tilesWithoutDoors
      .map {
        case (position, CorridorTile) =>
          if(position.sides.exists(tilesWithoutDoors.get(_).exists(isRoomTile)))
            position -> DoorTile
          else
            position -> CorridorTile
        case other => other
      }

    ((topology, tilesWithoutDoors), varRng)
  }

}