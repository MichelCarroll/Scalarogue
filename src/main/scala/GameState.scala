


case class Player(position: Position, being: Being) {
  private val viewportRange = 5
  def viewport = Area(
    Position(position.x - viewportRange, position.y - viewportRange),
    Position(position.x + viewportRange, position.y + viewportRange)
  )
}

case class GameState(dungeon: Dungeon, player: Player, rng: RNG) {
  import PlayerCommand._

  def applyPlayerCommand(playerCommand: PlayerCommand): GameState = {

    def attemptNewPosition(newPosition: Position): GameState =
      if(dungeon.cells.get(newPosition).forall(_.passable))
        this.copy(player = player.copy(newPosition))
      else
        this

    playerCommand match {
      case Up => attemptNewPosition(player.position.up(1))
      case Down => attemptNewPosition(player.position.down(1))
      case Left => attemptNewPosition(player.position.left(1))
      case Right => attemptNewPosition(player.position.right(1))
    }
  }

}

object GameState {
  def start(seed: Long): GameState = {

    val gridSize = Size(50, 50)
    val rng = SimpleRNG(seed)
    val (randomTree, newRng) = BSPTree.buildRandomTree(minLeafSurface = 0.02, maxLeafSurface = 0.15, skewdnessCutoff = 0.8)(rng)
    val ((_, floorplan), newRng2) = FloorplanGenerator.generate(randomTree, gridSize)(newRng)
    val (dungeonEither, newRng3) = DungeonGenerator.generate(floorplan)(newRng2)

    dungeonEither match {
      case Right(dungeon) =>
        GameState(
          dungeon = dungeon,
          player = Player(dungeon.entrancePosition, Nugget),
          rng = newRng3
        )
      case Left(error) => throw new Exception("Dungeon generation failed")
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

