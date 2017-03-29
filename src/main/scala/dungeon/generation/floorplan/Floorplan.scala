
package dungeon.generation.floorplan

import dungeon.generation.floorplan.BSPTree._
import dungeon.generation._
import math._
import random.RNG
import random.RNG._

case class Floorplan(
                 positionedTiles: Map[Position, Tile],
                 topology: Floorplan.Topology,
                 size: Size
               )

object Floorplan {

  sealed trait Corridor
  case class HorizontalCorridor(area: Area) extends Corridor
  case class VerticalCorridor(area: Area) extends Corridor

  case class Room(area: Area)

  case class Topology(rooms: Set[Room], corridors: Set[Corridor]) {
    def +(other: Topology) = this.copy(rooms = rooms ++ other.rooms, corridors = corridors ++ other.corridors)
    def +(room: Room) = this.copy(rooms = rooms + room)
    def +(corridor: Corridor) = this.copy(corridors = corridors + corridor)
  }

  def generate(tree: BSPTree, gridSize: Size, roomPadding: Int = 1): Rand[Floorplan] = rng => {
    import Direction._
    var varRng = rng

    def innerTopology(tree: BSPTree, area: Area): Topology = {

      def roomTouchingCenterOfSubspace(area: Area): Rand[Room] = rng => {
        val (widthRatio, newRng1) = RNG.nextPositiveGaussianRatio(5)(rng)
        val (heightRatio, newRng2) = RNG.nextPositiveGaussianRatio(5)(newRng1)

        val roomSize = Size(
          Math.max(1, ((area.size.width - roomPadding * 2) * (1 - widthRatio)).round.toInt),
          Math.max(1, ((area.size.height - roomPadding * 2) * (1 - heightRatio)).round.toInt)
        )

        val minX = Math.max(area.minX + roomPadding, area.center.x - roomSize.width + 1)
        val minY = Math.max(area.minY + roomPadding, area.center.y - roomSize.height + 1)
        val maxX = Math.min(area.center.x, area.maxX - roomSize.width + 1 - roomPadding)
        val maxY = Math.min(area.center.y, area.maxY - roomSize.height + 1 - roomPadding)

        val (roomXPos, newRng3) = RNG.nextPositiveInt(maxX - minX).map(_ + minX)(newRng2)
        val (roomYPos, newRng4) = RNG.nextPositiveInt(maxY - minY).map(_ + minY)(newRng3)

        (Room(Area(Position(roomXPos, roomYPos), roomSize)), newRng4)
      }

      tree match {

        case HorizontalBranch(left, right) =>
          val leftArea = Area(area.position, left.size)
          val rightArea = Area(area.position.right(left.size.width), right.size)
          innerTopology(left, leftArea) +
            innerTopology(right, rightArea) +
            HorizontalCorridor(Area(leftArea.center, rightArea.center))

        case VerticalBranch(top, bottom) =>
          val topArea = Area(area.position, top.size)
          val bottomArea = Area(area.position.down(top.size.height), bottom.size)
          innerTopology(top, topArea) +
            innerTopology(bottom, bottomArea) +
            VerticalCorridor(Area(topArea.center, bottomArea.center))

        case Leaf(size) =>
          val (newRoom, newRng) = roomTouchingCenterOfSubspace(area)(varRng)
          varRng = newRng
          Topology(rooms = Set(newRoom), corridors = Set())
      }

    }

    val topology = innerTopology(tree, Area(Position(0, 0), gridSize))

    val horizontallyRoomAdjacentPositions = topology.rooms
      .flatMap(room => Set(Left, Right).map(room.area.adjacencyLine))
      .flatMap(_.positions)

    val verticallyRoomAdjacentPositions = topology.rooms
      .flatMap(room => Set(Up, Down).map(room.area.adjacencyLine))
      .flatMap(_.positions)

    val tilesWithoutDoors: Map[Position, Tile] =
      topology.corridors.flatMap {
        case VerticalCorridor(area) =>
          area.positions.map(position =>
            if (horizontallyRoomAdjacentPositions.contains(position))
              position -> RoomTile
            else
              position -> CorridorTile
          )

        case HorizontalCorridor(area) =>
          area.positions.map(position =>
            if (verticallyRoomAdjacentPositions.contains(position))
              position -> RoomTile
            else
              position -> CorridorTile
          )
      }
        .groupBy(_._1)
        .map {
          case (_, tileMappings) => tileMappings
            .find(tileMapping => tileMapping._2.isRoomTile) //put priority over room tiles
            .getOrElse(tileMappings.head)
        } ++ topology.rooms.flatMap(_.area.positions.map(_ -> RoomTile)).toMap


    val tiles: Map[Position, Tile] = tilesWithoutDoors
      .map {
        case (position, CorridorTile) =>
          if (position.sides.exists(tilesWithoutDoors.get(_).exists(_.isRoomTile)))
            position -> DoorTile
          else
            position -> CorridorTile
        case other => other
      }


    (Floorplan(tiles, topology, gridSize), varRng)
  }
}