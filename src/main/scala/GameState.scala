import DungeonGenerator.GenerationError
import RNG.Rand


case class Notification(message: String)

case class GameState(dungeon: Dungeon, player: Player, rng: RNG) {
  import PlayerCommand._

  def applyPlayerCommand(playerCommand: PlayerCommand): (List[Notification], GameState) = {

    def attemptNewPosition(position: Position): (List[Notification], GameState) =
      dungeon.cells.get(position) match {

        case Some(OpenCell(Some(enemy: Enemy), structure, itemOnGround)) =>
          (
            enemy.drop match {
              case Some(item) => List(Notification(s"You slay a ${enemy.name}, and he drops ${item.amount} ${item.name}!"))
              case None => List(Notification(s"You slay a ${enemy.name}!"))
            },
            this.copy(dungeon = dungeon.withUpdatedCell(position,
              OpenCell(None, structure, enemy.drop.map(itemOnGround + _).getOrElse(itemOnGround))
            ))
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
              player = player.copy(position),
              dungeon = dungeon.withUpdatedCell(position,
                OpenCell(None, structure, Set())
              )
            )
          )

        case _ =>
          (List(), this)

      }

    playerCommand match {
      case Up => attemptNewPosition(player.position.up(1))
      case Down => attemptNewPosition(player.position.down(1))
      case Left => attemptNewPosition(player.position.left(1))
      case Right => attemptNewPosition(player.position.right(1))
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
          player = Player(dungeon.entrancePosition, Nugget),
          rng = newRng
        )
      case (Left(error), _) => throw new Exception("Dungeon generation failed")
    }

  }
}


sealed trait PlayerCommand
object PlayerCommand {
  case object Up extends PlayerCommand
  case object Down extends PlayerCommand
  case object Right extends PlayerCommand
  case object Left extends PlayerCommand

  def fromKeyCode(keyCode: Int): Option[PlayerCommand] = keyCode match {
    case 37 => Some(PlayerCommand.Left)
    case 38 => Some(PlayerCommand.Up)
    case 39 => Some(PlayerCommand.Right)
    case 40 => Some(PlayerCommand.Down)
    case _  => None
  }
}

