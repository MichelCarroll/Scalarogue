

import DungeonGenerator.GenerationError
import dungeon.generation.bsp.BSPTree
import math.{Position, RNG, Size}
import math.RNG.Rand

case class Notification(message: String)

case class GameState(dungeon: Dungeon, rng: RNG) {
  import Command._

  def applyCommand(positionedBeing: PositionedBeing, command: Command): GameTransition = {

    def attemptNewPosition(position: Position): GameTransition =
      dungeon.cells.get(position) match {

        case Some(cell@OpenCell(Some(being: Being), structure, itemsOnGround)) =>
          HitTransition(positionedBeing.being, being, cell, position, this)

        case Some(OpenCell(None, Some(structure: Openable), items)) =>
          OpenTransition(positionedBeing.being, OpenCell(None, Some(structure.opened), items), position, this)

        case Some(cell@OpenCell(None, structure, items)) if cell.passable =>
          MoveTransition(positionedBeing.being, positionedBeing.position, cell, position, OpenCell(Some(positionedBeing.being), structure, Set()), items, this)

        case _ =>
          IdentityTransition(this)

      }

    command match {
      case Up => attemptNewPosition(positionedBeing.position.up(1))
      case Down => attemptNewPosition(positionedBeing.position.down(1))
      case Left => attemptNewPosition(positionedBeing.position.left(1))
      case Right => attemptNewPosition(positionedBeing.position.right(1))
    }
  }

}

trait GameTransition {
  def newState: GameState
  def notifications: List[Notification]
}

case class IdentityTransition(state: GameState) extends GameTransition {
  def newState = state
  def notifications = List()
}

case class MoveTransition(subject: Being, oldCellPosition: Position, oldCell: OpenCell, newCellPosition: Position, newCell: OpenCell, items: Set[Item], state: GameState) extends GameTransition  {

  val verb = if(subject.descriptor.isThirdPerson) "picks up" else "pick up"
  def notifications = items.map(item => Notification(s"${subject.descriptor.name} $verb ${item.amount} ${item.name}")).toList

  def newState =  state.copy(
    dungeon = state.dungeon
      .withRemovedBeing(oldCellPosition)
      .withUpdatedCell(newCellPosition, newCell)
  )

}

case class OpenTransition(subject: Being, newCell: Cell, position: Position, state: GameState) extends GameTransition  {
  val verb = if(subject.descriptor.isThirdPerson) "opens" else "open"
  def notifications = List(Notification(s"${subject.descriptor.name} $verb a door"))
  def newState = state.copy(dungeon = state.dungeon.withUpdatedCell(position, newCell))
}

case class HitTransition(sourceBeing: Being, targetBeing: Being, targetCell: OpenCell, targetBeingPosition: Position, state: GameState) extends GameTransition  {

  val hitVerb = if(sourceBeing.descriptor.isThirdPerson) "hits" else "hit"
  val slayVerb = if(sourceBeing.descriptor.isThirdPerson) "slays" else "slay"

  val (damage, newRng) = sourceBeing.descriptor.damageRange.randomDamage(state.rng)
  val newBeing = targetBeing.hit(damage)
  val damageNotification = Notification(s"${sourceBeing.descriptor.name} $hitVerb ${newBeing.descriptor.name} for ${damage.value} damage!")

  def notifications =
    if(newBeing.dead)
      newBeing.descriptor.drop match {
        case Some(item) => List(damageNotification, Notification(s"${sourceBeing.descriptor.name} $slayVerb ${newBeing.descriptor.name}, and ${targetBeing.descriptor.pronoun} drops ${item.amount} ${item.name}!"))
        case None => List(damageNotification, Notification(s"${sourceBeing.descriptor.name} $slayVerb ${newBeing.descriptor.name}!"))
      }
    else List(damageNotification)

  def newState = {
    val updatedDungeon = if(newBeing.dead)
      state.dungeon.withUpdatedCell(targetBeingPosition,
        OpenCell(None, targetCell.structure, newBeing.descriptor.drop.map(targetCell.item + _).getOrElse(targetCell.item))
      )
    else
      state.dungeon.withUpdatedCell(targetBeingPosition, OpenCell(Some(newBeing), targetCell.structure, targetCell.item))

    state.copy(dungeon = updatedDungeon, rng = newRng)
  }

}

object GameState {

  def generatedDungeon: Rand[Either[GenerationError, Dungeon]] = rng => {
    val gridSize = Size(50, 50)
    val (randomTree, newRng) = BSPTree.buildRandomTree(size = gridSize)(rng)
    val ((_, floorplan), newRng2) = FloorplanGenerator.generate(randomTree, gridSize)(newRng)
    val (dungeonEither, newRng3) = DungeonGenerator.generate(floorplan)(newRng2)
    (dungeonEither, newRng3)
  }

  def start: Rand[GameState] = rng => {

    generatedDungeon(rng) match {
      case (Right(dungeon), newRng) =>
        (
          GameState(
            dungeon = dungeon,
            rng = newRng
          ),
          newRng
        )
      case (Left(error), _) => throw new Exception("Dungeon generation failed")
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

