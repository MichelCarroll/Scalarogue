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
          case x if x < 0.015 =>  Spider.randomNewBeing.map(being => Cell(being = Some(being)))
          case x if x < 0.025 =>  unit(Cell(itemBag = ItemBag(HealthPotion -> 1)))
          case x if x < 0.035 =>  unit(Cell(itemBag = ItemBag(Sword -> 1)))
          case _ => unit(Cell())
        }

      val randomEntranceAndExit: Rand[(Position, Position)] =
        nextsFromSet(roomTiles, 2)
          .map(positionedTile =>
            (positionedTile.head._1, positionedTile.tail.head._1)
          )

      val randomFloorplanCells: Rand[Map[Position, Cell]] = {
        val listOfRandomCells = floorplan.positionedTiles.mapValues {
          case DoorTile => unit(Cell(structure = Some(ClosedDoor)))
          case RoomTile => randomRoomCell
          case tile => unit(Cell())
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
              cells = floorplanCells
                + (entrancePosition -> Cell(being = Some(player), structure = Some(Upstairs)))
                + (exitPosition -> Cell(structure = Some(Downstairs))),
              area = Area(Position(0,0), floorplan.size),
              entrancePosition = entrancePosition
            ))
        }

    }
  }

}
