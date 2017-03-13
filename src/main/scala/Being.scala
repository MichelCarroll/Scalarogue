
import math.{Area, Direction, Position, RNG}
import math.RNG.Rand

trait Intelligence {
  def nextCommand(positionedBeing: PositionedBeing, dungeon: Dungeon): Rand[(Option[Command], Intelligence)]
}

case class NoIntelligence() extends Intelligence {
  def nextCommand(positionedBeing: PositionedBeing, dungeon: Dungeon) = RNG.unit(None, this)
}

case class RandomIntelligence() extends Intelligence {
  def nextCommand(positionedBeing: PositionedBeing, dungeon: Dungeon) =
    RNG.map(RNG.nextFromSet(Command.all))(x => (x, this))
}

case class SimpleAgroIntelligence(maxRange: Int) extends Intelligence {
  def nextCommand(positionedBeing: PositionedBeing, dungeon: Dungeon) = {
    RNG.unit(
      dungeon.positionedPlayer.flatMap(player =>
        if(player.position.manhattanDistanceTo(positionedBeing.position) <= maxRange)
          dungeon.bestDirectionTo(positionedBeing.position, player.position).map {
            case Direction.Up => Command.Up
            case Direction.Down => Command.Down
            case Direction.Right => Command.Right
            case Direction.Left => Command.Left
          }
        else
          None
      )
      ,this
    )
  }
}

sealed trait BeingDescriptor {
  def name: String
  def pronoun: String
  def drop: Option[Item]
  def isThirdPerson = true
  def damageRange: DamageRange
  def maxHealth: Health
}

case object SpiderGenerator {
  def generate = Being(Spider, Spider.maxHealth, SimpleAgroIntelligence(maxRange = 4))
}

case object Spider extends BeingDescriptor {
  def name = "a spider"
  def pronoun = "it"
  def drop = Some(Gold(5))
  def damageRange = DamageRange(Damage(1), Damage(2))
  def maxHealth = Health(5)
}

case class Being(descriptor: BeingDescriptor, health: Health, intelligence: Intelligence) {

  def hit(damage: Damage) = Being(descriptor, health - damage, intelligence)

  def dead = health.value <= 0

  def withNextCommand(position: Position, dungeon: Dungeon): Rand[(Option[Command], Being)] = rng => {
    val ((nextCommand, nextIntelligence), nextRng) = intelligence.nextCommand(PositionedBeing(position, this), dungeon)(rng)
    ((nextCommand, Being(descriptor, health, nextIntelligence)), nextRng)
  }
}

case object PlayerGenerator {
  def generate = Being(Player, Player.maxHealth, NoIntelligence())
}

case object Player extends BeingDescriptor with Sighted {

  override def isThirdPerson = false
  val name = "you"
  val pronoun = "your"
  def drop = None
  def damageRange = DamageRange(Damage(2), Damage(4))
  def maxHealth = Health(20)

  private val viewportRange = 6
  val lineOfLightRange = Math.ceil(Math.sqrt(2 * Math.pow(viewportRange, 2)))

  def viewport(position: Position) = Area(
    Position(position.x - viewportRange + 1, position.y - viewportRange + 1),
    Position(position.x + viewportRange - 1, position.y + viewportRange - 1)
  )
}
