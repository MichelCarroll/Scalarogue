package dungeon.generation

import dungeon._
import dungeon.generation.floorplan.Floorplan
import game._
import game.being.{Player, Spider}
import random.RNG
import random.RNG._
import math._


object DungeonGenerator {

  trait GenerationError
  case object NeedAtLeastTwoRoomTiles extends GenerationError

  def generate(floorplan: Floorplan): Rand[Either[GenerationError, Dungeon]] = {
    val roomTiles = floorplan.positionedTiles.filter(_._2 == RoomTile).toSet

    if(roomTiles.size < 2)
      unit(Left(NeedAtLeastTwoRoomTiles))

    else {

      def randomRoomCell: Rand[Cell] =
        nextRatio.flatMap {
          case x if x < 0.015 =>  Spider.randomNewBeing.map(being => OpenCell(being = Some(being)))
          case _ => unit(OpenCell())
        }

      val randomEntranceAndExit: Rand[(Position, Position)] =
        nextsFromSet(roomTiles, 2)
          .map(positionedTile =>
            (positionedTile.head._1, positionedTile.tail.head._1)
          )

      val wallCells: Map[Position, Cell] =
        Area(Position(0,0), floorplan.size).positions.map(_ -> ClosedCell).toMap

      val randomFloorplanCells: Rand[Map[Position, Cell]] = {
        val listOfRandomCells = floorplan.positionedTiles.mapValues {
          case DoorTile => unit(OpenCell(structure = Some(ClosedDoor)))
          case RoomTile => randomRoomCell
          case tile => unit(OpenCell())
        }
        RNG.sequence(listOfRandomCells.toList
          .map(x => unit(x._1).combine(x._2))
        ).map(_.toMap)
      }

      randomFloorplanCells
        .combine(randomEntranceAndExit)
        .combine(Player.randomNewBeing)
        .map {
          case ((floorplanCells, (entrancePosition, exitPosition)), player) =>
            Right(Dungeon(
              cells = wallCells ++ floorplanCells
                + (entrancePosition -> OpenCell(being = Some(player), structure = Some(Upstairs)))
                + (exitPosition -> OpenCell(structure = Some(Downstairs))),
              area = Area(Position(0,0), floorplan.size),
              entrancePosition = entrancePosition
            ))
        }

    }
  }

}
