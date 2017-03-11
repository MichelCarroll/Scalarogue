

import DungeonGenerator.GenerationError
import RNG.Rand


case class Notification(message: String)

case class GameState(dungeon: Dungeon, rng: RNG) {
  import Command._

  def applyCommand(positionedBeing: PositionedBeing, command: Command): (List[Notification], GameState) = {

    def attemptNewPosition(position: Position): (List[Notification], GameState) =
      dungeon.cells.get(position) match {

        case Some(OpenCell(Some(being: Being), structure, itemsOnGround)) =>
          val damage = 1
          val newBeing = being.hit(damage)
          val damageNotification = Notification(s"You hit the ${newBeing.descriptor.name} for $damage damage!")
          if(newBeing.dead) (
              newBeing.descriptor.drop match {
                case Some(item) => List(damageNotification, Notification(s"You slay a ${newBeing.descriptor.name}, and he drops ${item.amount} ${item.name}!"))
                case None => List(damageNotification, Notification(s"You slay a ${newBeing.descriptor.name}!"))
              },
              this.copy(dungeon = dungeon.withUpdatedCell(position,
                OpenCell(None, structure, being.descriptor.drop.map(itemsOnGround + _).getOrElse(itemsOnGround))
              ))
          ) else (
            List(damageNotification),
            this.copy(dungeon = dungeon.withUpdatedCell(position, OpenCell(Some(newBeing), structure, itemsOnGround)))
          )

        case Some(OpenCell(None, Some(structure: Openable), items)) =>
          (
            List(Notification(s"You open a door")),
            this.copy(dungeon = dungeon.withUpdatedCell(position,
              OpenCell(None, Some(structure.opened), items)
            ))
          )

        case Some(cell@OpenCell(None, structure, items)) if cell.passable =>
          (
            items.map(item => Notification(s"You pick up ${item.amount} ${item.name}")).toList,
            this.copy(
              dungeon = dungeon
                .withRemovedBeing(positionedBeing.position)
                .withUpdatedCell(position, OpenCell(Some(positionedBeing.being), structure, Set()))
            )
          )

        case _ =>
          (List(), this)

      }

    command match {
      case Up => attemptNewPosition(positionedBeing.position.up(1))
      case Down => attemptNewPosition(positionedBeing.position.down(1))
      case Left => attemptNewPosition(positionedBeing.position.left(1))
      case Right => attemptNewPosition(positionedBeing.position.right(1))
    }
  }

}

object GameState {

  def generatedDungeon: Rand[Either[GenerationError, Dungeon]] = rng => {
    val gridSize = Size(50, 50)
    val (randomTree, newRng) = BSPTree.buildRandomTree(minLeafSurface = 0.02, maxLeafSurface = 0.15, skewdnessCutoff = 0.8)(rng)
    val ((_, floorplan), newRng2) = FloorplanGenerator.generate(randomTree, gridSize)(newRng)
    val (dungeonEither, newRng3) = DungeonGenerator.generate(floorplan)(newRng2)
    (dungeonEither, newRng3)
  }

  def start(seed: Long): GameState = {

    generatedDungeon(SimpleRNG(seed)) match {
      case (Right(dungeon), newRng) =>
        GameState(
          dungeon = dungeon,
          rng = newRng
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
}

