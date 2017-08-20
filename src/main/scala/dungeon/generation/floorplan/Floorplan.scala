
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

  def generate(tree: BSPTree, gridSize: Size, roomPadding: Int = 1): Rand[Floorplan] = {
    import Direction._

    def innerTopology(tree: BSPTree, area: Area): Rand[Topology] = {

      def roomTouchingCenterOfSubspace(area: Area): Rand[Room] =
        RNG.nextPositiveGaussianRatio(5)
          .combine(RNG.nextPositiveGaussianRatio(5))
          .map { case (widthRatio, heightRatio) => Size(
              Math.max(1, ((area.size.width - roomPadding * 2) * (1 - widthRatio)).round.toInt),
              Math.max(1, ((area.size.height - roomPadding * 2) * (1 - heightRatio)).round.toInt)
            )
          }
          .flatMap { roomSize =>
            val minX = Math.max(area.minX + roomPadding, area.center.x - roomSize.width + 1)
            val minY = Math.max(area.minY + roomPadding, area.center.y - roomSize.height + 1)
            val maxX = Math.min(area.center.x, area.maxX - roomSize.width + 1 - roomPadding)
            val maxY = Math.min(area.center.y, area.maxY - roomSize.height + 1 - roomPadding)

            for {
              roomXPos <- RNG.nextPositiveInt(maxX - minX).map(_ + minX)
              roomYPos <- RNG.nextPositiveInt(maxY - minY).map(_ + minY)
            } yield Room(Area(Position(roomXPos, roomYPos), roomSize))
          }

      tree match {

        case HorizontalBranch(left, right) =>
          val leftArea = Area(area.position, left.size)
          val rightArea = Area(area.position.right(left.size.width), right.size)
          for {
            leftTopology <- innerTopology(left, leftArea)
            rightTopology <- innerTopology(right, rightArea)
          } yield
            leftTopology +
            rightTopology +
            HorizontalCorridor(Area(leftArea.center, rightArea.center))

        case VerticalBranch(top, bottom) =>
          val topArea = Area(area.position, top.size)
          val bottomArea = Area(area.position.down(top.size.height), bottom.size)
          for {
            topTopology    <- innerTopology(top, topArea)
            bottomTopology <- innerTopology(bottom, bottomArea)
          } yield
            topTopology +
            bottomTopology +
            VerticalCorridor(Area(topArea.center, bottomArea.center))

        case Leaf(size) =>
          roomTouchingCenterOfSubspace(area)
            .map(newRoom => Topology(rooms = Set(newRoom), corridors = Set()))

      }

    }

    val randomTopology = innerTopology(tree, Area(Position(0, 0), gridSize))

    def horizontallyRoomAdjacentPositions(topology: Topology) =
      topology.rooms
        .flatMap(room => Set(Left, Right).map(room.area.adjacencyLine))
        .flatMap(_.positions)

    def verticallyRoomAdjacentPositions(topology: Topology) =
      topology.rooms
        .flatMap(room => Set(Up, Down).map(room.area.adjacencyLine))
        .flatMap(_.positions)

    def tilesWithoutDoors(topology: Topology): Map[Position, Tile] = {
      val horRoomAdjacentPositions = horizontallyRoomAdjacentPositions(topology)
      val verRoomAdjacentPositions = verticallyRoomAdjacentPositions(topology)
      topology.corridors.flatMap {
        case VerticalCorridor(area) =>
          area.positions.map(position =>
            if (horRoomAdjacentPositions.contains(position))
              position -> RoomTile
            else
              position -> CorridorTile
          )

        case HorizontalCorridor(area) =>
          area.positions.map(position =>
            if (verRoomAdjacentPositions.contains(position))
              position -> RoomTile
            else
              position -> CorridorTile
          )
      }
        .groupBy(_._1)
        .map {
          case (_, tileMappings) => tileMappings
            .find(tileMapping => tileMapping._2 == RoomTile) //put priority over room tiles
            .getOrElse(tileMappings.head)
        } ++ topology.rooms.flatMap(_.area.positions.map(_ -> RoomTile)).toMap
    }

    def tiles(topology: Topology): Map[Position, Tile] = {
      val withoutDoors = tilesWithoutDoors(topology)
      withoutDoors
        .map {
          case (position, CorridorTile) =>
            if (position.sides.exists(withoutDoors.get(_).exists(_ == RoomTile)))
              position -> DoorTile
            else
              position -> CorridorTile
          case other => other
        }
    }

    randomTopology.map(topology => Floorplan(tiles(topology), topology, gridSize))
  }
}