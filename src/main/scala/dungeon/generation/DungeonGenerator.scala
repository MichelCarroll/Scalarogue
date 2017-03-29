package dungeon.generation

import dungeon._
import dungeon.generation.floorplan.Floorplan
import game._
import game.being.{PlayerGenerator, SpiderGenerator}
import random.RNG._
import math._


object DungeonGenerator {

  trait GenerationError
  case object NeedAtLeastTwoRoomTiles extends GenerationError

  def generate(floorplan: Floorplan): Rand[Either[GenerationError, Dungeon]] = rng => {
    var varRng = rng
    val roomTiles = floorplan.positionedTiles.filter(_._2.isRoomTile).toSet

    if(roomTiles.size < 2)
      (Left(NeedAtLeastTwoRoomTiles), rng)

    else {

      def randomRoomCell: Cell = {
        val (p, newRng) = nextRatio(varRng)
        varRng = newRng

        p match {
          case x if x < 0.0005 => OpenCell(structure = Some(Upstairs))
          case x if x < 0.001 =>  OpenCell(structure = Some(Downstairs))
          case x if x < 0.015 =>  OpenCell(being = Some(SpiderGenerator.generate))
          case _ => OpenCell()
        }
      }

      val entranceAndExit: (Position, Position) = {
        val (positionedTile, newRng) = nextsFromSet(roomTiles, 2)(rng)
        varRng = newRng
        (positionedTile.head._1, positionedTile.tail.head._1)
      }
      val entrancePosition = entranceAndExit._1
      val exitPosition = entranceAndExit._2

      val wallCells: Map[Position, Cell] =
        Area(Position(0,0), floorplan.size).positions.map(_ -> ClosedCell).toMap

      val floorplanCells = floorplan.positionedTiles.mapValues {
        case DoorTile => OpenCell(structure = Some(ClosedDoor))
        case RoomTile => randomRoomCell
        case tile => OpenCell()
      }

      val dungeon = Dungeon(
        cells = wallCells ++ floorplanCells
          + (entrancePosition -> OpenCell(being = Some(PlayerGenerator.generate), structure = Some(Upstairs)))
          + (exitPosition -> OpenCell(structure = Some(Downstairs))),
        area = Area(Position(0,0), floorplan.size),
        entrancePosition = entrancePosition
      )
      (Right(dungeon), rng)
    }
  }

}
