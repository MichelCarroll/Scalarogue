
import RNG.Rand

case class PositionedBeing(position: Position, being: Being)

case class Dungeon(cells: Map[Position, Cell], area: Area, entrancePosition: Position) extends Navigatable {

  def withUpdatedBeing(positionedBeing: PositionedBeing) =  copy(
    cells = cells.get(positionedBeing.position) match {
      case Some(OpenCell(_, structure, items)) => cells.updated(positionedBeing.position, OpenCell(Some(positionedBeing.being), structure, items))
      case _ => cells
    }
  )

  def withUpdatedCell(at: Position, cell: Cell) = copy(
    cells = cells.updated(at, cell)
  )

  def withRemovedBeing(at: Position) = copy(
    cells = cells.get(at) match {
      case Some(OpenCell(_, structure, items)) => cells.updated(at, OpenCell(None, structure, items))
      case _ => cells
    }
  )

  def positionedPlayer: Option[PositionedBeing] = cells
    .map {
      case (position, OpenCell(Some(player@Being(Player, _, _)), _, _)) => Some(PositionedBeing(position, player))
      case _ => None
    }
    .find(_.isDefined)
    .flatten


  def positionedBeings(target: BeingDescriptor) = cells
    .flatMap {
      case (position, OpenCell(Some(being@Being(descriptor, _, _)), _, _)) if descriptor == target => Some(PositionedBeing(position, being))
      case _ => None
    }
    .toSet


}

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
        val (p, newRng) = RNG.nextRatio(varRng)
        varRng = newRng

        p match {
          case x if x < 0.0005 => OpenCell(structure = Some(Upstairs))
          case x if x < 0.001 =>  OpenCell(structure = Some(Downstairs))
          case x if x < 0.015 =>  OpenCell(being = Some(SpiderGenerator.generate))
          case _ => OpenCell()
        }
      }

      val entranceAndExit: (Position, Position) = {
        val (positionedTile, newRng) = RNG.nextsFromSet(roomTiles, 2)(rng)
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
