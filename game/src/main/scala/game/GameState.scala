package game


import dungeon.{Cell, Dungeon, OpenCell}
import dungeon.generation.DungeonGenerator
import dungeon.generation.DungeonGenerator.GenerationError
import dungeon.generation.floorplan.{BSPTree, Floorplan, RandomBSPTreeParameters}
import game.being.{Being, Player}
import math.{Position, Size}
import primitives.Ratio
import random.RNG
import random.RNG._
import com.softwaremill.quicklens._


trait GameTransition {
  def newState: GameState
}

case class MoveTransition(subject: Being, oldCellPosition: Position, newCellPosition: Position, newCellBeing: Being, newCellItems: ItemBag, newCellStructure: Option[Structure], state: GameState) extends GameTransition  {

  val notifications = newCellItems.items.map { case (item, amount) => TargetTaken(subject.descriptor, amount, item) }.toList

  def newState = state
    .modify(_.dungeon.cells.at(oldCellPosition)).using {
      case OpenCell(_, structure, items) => OpenCell(None, structure, items)
    }
    .modify(_.dungeon.cells.at(newCellPosition)).using {
      case OpenCell(_, structure, items) => OpenCell(
        being = Some(newCellBeing.copy(itemBag = newCellBeing.itemBag + newCellItems)),
        structure = newCellStructure,
        itemBag = ItemBag.empty
      )
    }
    .modify(_.notificationHistory).using(notifications ++: _)

}

case class RefreshRevealedPositionsTransition(state: GameState) extends GameTransition {

  def newState = state.dungeon.playerPosition match {
    case Some(position) =>
      state.copy(revealedPositions = state.revealedPositions ++ Player.positionsWithinRangeTouchedByPerimeterRay(position, state.dungeon))
    case _ => state
  }

}


case class HitTransition(sourceBeing: Being, targetBeing: Being, targetCell: OpenCell, targetBeingPosition: Position, state: GameState) extends GameTransition  {

  val (((newBeing, notificationOpt), damage), newRng) = sourceBeing.descriptor.damageRange
    .randomDamage
    .flatMap(damage => targetBeing.hit(damage).map((_, damage)))(state.rng)

  def hitNotification = TargetHit(sourceBeing.descriptor, newBeing.descriptor, notificationOpt)
  def deathNotifications =
    if(newBeing.body.dead)
      TargetDies(newBeing.descriptor) :: newBeing.itemBag.items.map {
        case (item, amount) => TargetDropsItem(targetBeing.descriptor, amount, item)
      }.toList
    else List()

  val notifications = hitNotification :: deathNotifications

  def newState =
    (
      if (newBeing.body.dead)
        state.modify(_.dungeon.cells.at(targetBeingPosition)).setTo(
          OpenCell(None, targetCell.structure, targetCell.itemBag + newBeing.itemBag)
        )
      else
        state.modify(_.dungeon.cells.at(targetBeingPosition)).setTo(
          OpenCell(Some(newBeing), targetCell.structure, targetCell.itemBag)
        )
    )
    .modify(_.rng).setTo(newRng)
    .modify(_.notificationHistory).using(notifications ++: _)

}

case class GameState(dungeon: Dungeon, rng: RNG, revealedPositions: Set[Position], notificationHistory: List[Notification]) {
  import Command._

  def applyCommand(sourcePosition: Position, command: Command): GameState = {

    def attemptNewPosition(sourceBeing: Being, destinationPosition: Position): GameState =
      dungeon.cells.get(destinationPosition) match {

        case Some(cell@OpenCell(Some(being: Being), structure, itemsOnGround)) =>
          HitTransition(sourceBeing, being, cell, destinationPosition, this).newState

        case Some(cell@OpenCell(None, Some(openable: Openable), items)) =>
          this
            .modify(_.dungeon.cells.at(destinationPosition)).using {
              case cell@OpenCell(_,_,_) => cell.modify(_.structure).setTo(Some(openable.opened))
            }
            .modify(_.notificationHistory).using(TargetOpened(sourceBeing.descriptor, openable) +: _)

        case Some(cell@OpenCell(None, structure, items)) if cell.passable =>
          MoveTransition(sourceBeing, sourcePosition, destinationPosition, sourceBeing, items, structure, this).newState

        case _ => this

      }

    dungeon.cells.get(sourcePosition) match {
      case Some(OpenCell(Some(being@Being(_,_,_,_)), _, _)) => command match {
          case Up => attemptNewPosition(being, sourcePosition.up(1))
          case Down => attemptNewPosition(being, sourcePosition.down(1))
          case Left => attemptNewPosition(being, sourcePosition.left(1))
          case Right => attemptNewPosition(being, sourcePosition.right(1))
          case UseItem(itemSlug) => this
        }
      case _ => throw new Exception("No being in this tile")
    }
  }


}





sealed trait Command
object Command {
  case object Up extends Command
  case object Down extends Command
  case object Right extends Command
  case object Left extends Command
  case class UseItem(itemSlug: ItemSlug) extends Command

  def fromKeyCode(keyCode: Int): Option[Command] = keyCode match {
    case 37 => Some(Command.Left)
    case 38 => Some(Command.Up)
    case 39 => Some(Command.Right)
    case 40 => Some(Command.Down)
    case _  => None
  }

  def all: Set[Command] = Set(Up, Down, Right, Left)
}


/**
  * Created by MichelCarroll on 3/28/2017.
  */
object GameState {

  def generatedDungeon: Rand[Either[GenerationError, Dungeon]] = {
    val gridSize = Size(50, 50)
    for {
      randomTree <- BSPTree.generate(
        RandomBSPTreeParameters(
          size = gridSize,
          minLeafEdgeLength = 3,
          minLeafSurfaceRelativeToTotal = Ratio(0.2)
        )
      )
      floorplan <- Floorplan.generate(randomTree, gridSize)
      dungeonEither <- DungeonGenerator.generate(floorplan)
    } yield dungeonEither
  }

  def start: Rand[GameState] = rng => {

    generatedDungeon(rng) match {
      case (Right(dungeon), newRng) => dungeon.playerPosition match {

        case Some(position) =>(
          GameState(
            dungeon = dungeon,
            rng = newRng,
            revealedPositions = Player.positionsWithinRangeTouchedByPerimeterRay(position, dungeon),
            notificationHistory = List()
          ),
          newRng
          )

        case None => throw new Exception("Dungeon needs a player")
      }
      case (Left(error), _) => throw new Exception("Dungeon generation failed")
    }

  }
}
