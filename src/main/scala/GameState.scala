


case class Player(position: Position, being: Being) {

  private val viewportRange = 6
  val lineOfLightRange = Math.ceil(Math.sqrt(2 * Math.pow(viewportRange, 2)))

  def viewport = Area(
    Position(position.x - viewportRange + 1, position.y - viewportRange + 1),
    Position(position.x + viewportRange - 1, position.y + viewportRange - 1)
  )
}

case class Notification(message: String)

case class GameState(dungeon: Dungeon, player: Player, rng: RNG) {
  import PlayerCommand._

  def applyPlayerCommand(playerCommand: PlayerCommand): (List[Notification], GameState) = {

    def attemptNewPosition(newPosition: Position): (List[Notification], GameState) =
      dungeon.cells.get(newPosition) match {

        case Some(OpenCell(Some(being: Enemy), structure, item)) =>
          (
            List(Notification(s"You slay a ${being.name}!")),
            this.copy(dungeon = dungeon.copy(dungeon.cells.updated(newPosition, OpenCell(None, structure, item))))
          )

        case Some(OpenCell(being, Some(structure: Openable), item)) =>
          (
            List(Notification(s"You open a door")),
            this.copy(dungeon = dungeon.copy(dungeon.cells.updated(newPosition, OpenCell(being, Some(structure.opened), item))))
          )

        case Some(cell) if cell.passable =>
          (List(), this.copy(player = player.copy(newPosition)))

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

  def perimeterRays = {
    val nRays = 80
    val interval = Math.PI * 2 / nRays

    (0 until nRays)
      .map(_ * interval)
      .map(theta => Vector(player.lineOfLightRange, Angle(theta)))
      .map(vector => Ray(Vector(player.position.x + 0.5, player.position.y + 0.5), vector))
  }

  def positionsWithinRangeTouchedByPerimeterRay: Set[Position] = {
    var positionsTouched = Set[Position]()

    for(ray <- perimeterRays) {
      val increment = 0.8
      val delta = ray.vector.unit * increment
      val numberIncrements = (ray.vector.magnitude / increment).toInt

      var touchedWall = false

      for(i <- 0 to numberIncrements) {
        val currentPosition = (ray.origin + delta * i).position

        if(!touchedWall) {
          positionsTouched = positionsTouched + currentPosition

          if(dungeon.cells.get(currentPosition).forall(_.opaque)) {
            touchedWall = true
          }
        }

      }
    }

    positionsTouched
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

