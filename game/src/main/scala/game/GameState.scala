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


trait GameTransition {
  def newState: GameState
}

case class IdentityTransition(state: GameState) extends GameTransition {
  def newState = state
}

case class MoveTransition(subject: Being, oldCellPosition: Position, oldCell: OpenCell, newCellPosition: Position, newCell: OpenCell, items: Set[Item], state: GameState) extends GameTransition  {

  val notifications = items.map(item => TargetTaken(subject.descriptor, item)).toList

  def newState = state.copy(
    dungeon = state.dungeon
      .withRemovedBeing(oldCellPosition)
      .withUpdatedCell(newCellPosition, newCell),
    notificationHistory = notifications ++: state.notificationHistory
  )

}

case class RefreshRevealedPositionsTransition(state: GameState) extends GameTransition {

  def newState = state.dungeon.playerPosition match {
    case Some(position) =>
      state.copy(revealedPositions = state.revealedPositions ++ Player.positionsWithinRangeTouchedByPerimeterRay(position, state.dungeon))
    case _ => state
  }

}

case class OpenTransition(subject: Being, openable: Openable, newCell: Cell, position: Position, state: GameState) extends GameTransition  {
  def newState = state.copy(
    dungeon = state.dungeon.withUpdatedCell(position, newCell),
    notificationHistory = TargetOpened(subject.descriptor, openable) :: state.notificationHistory
  )
}

case class HitTransition(sourceBeing: Being, targetBeing: Being, targetCell: OpenCell, targetBeingPosition: Position, state: GameState) extends GameTransition  {

  val (((newBeing, notificationOpt), damage), newRng) = sourceBeing.descriptor.damageRange
    .randomDamage
    .flatMap(damage => targetBeing.hit(damage).map((_, damage)))(state.rng)

  def hitNotification = TargetHit(sourceBeing.descriptor, newBeing.descriptor, notificationOpt)
  def deathNotifications =
    if(newBeing.body.dead)
      newBeing.descriptor.drop match {
        case Some(item) => List(TargetDies(newBeing.descriptor), TargetDropsItem(targetBeing.descriptor, item))
        case None => List(TargetDies(newBeing.descriptor))
      }
    else List()

  val notifications = hitNotification :: deathNotifications

  def newState = {
    val updatedDungeon = if(newBeing.body.dead)
      state.dungeon.withUpdatedCell(targetBeingPosition,
        OpenCell(None, targetCell.structure, newBeing.descriptor.drop.map(targetCell.item + _).getOrElse(targetCell.item))
      )
    else
      state.dungeon.withUpdatedCell(targetBeingPosition, OpenCell(Some(newBeing), targetCell.structure, targetCell.item))

    state.copy(dungeon = updatedDungeon, rng = newRng, notificationHistory = notifications ++: state.notificationHistory)
  }

}

case class GameState(dungeon: Dungeon, rng: RNG, revealedPositions: Set[Position], notificationHistory: List[Notification]) {
  import Command._

  def applyCommand(sourcePosition: Position, command: Command): GameTransition = {

    def attemptNewPosition(sourceBeing: Being, destinationPosition: Position): GameTransition =
      dungeon.cells.get(destinationPosition) match {

        case Some(cell@OpenCell(Some(being: Being), structure, itemsOnGround)) =>
          HitTransition(sourceBeing, being, cell, destinationPosition, this)

        case Some(OpenCell(None, Some(openable: Openable), items)) =>
          OpenTransition(sourceBeing, openable, OpenCell(None, Some(openable.opened), items), destinationPosition, this)

        case Some(cell@OpenCell(None, structure, items)) if cell.passable =>
          MoveTransition(sourceBeing, sourcePosition, cell, destinationPosition, OpenCell(Some(sourceBeing), structure, Set()), items, this)

        case _ =>
          IdentityTransition(this)

      }


    dungeon.cells.get(sourcePosition) match {
      case Some(OpenCell(Some(being@Being(_,_,_)), _, _)) => command match {
          case Up => attemptNewPosition(being, sourcePosition.up(1))
          case Down => attemptNewPosition(being, sourcePosition.down(1))
          case Left => attemptNewPosition(being, sourcePosition.left(1))
          case Right => attemptNewPosition(being, sourcePosition.right(1))
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
