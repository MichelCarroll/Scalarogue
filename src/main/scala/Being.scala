



sealed trait BeingDescriptor {
  def name: String
  def pronoun: String
  def drop: Option[Item]
  def isThirdPerson = true
}

case object SpiderGenerator {
  def generate = Being(Spider, 10)
}

case object Spider extends BeingDescriptor {
  def name = "a spider"
  def pronoun = "it"
  def drop: Option[Item] = Some(Gold(5))
}

case class Being(descriptor: BeingDescriptor, health: Int) {
  def hit(damage: Int) = Being(descriptor, health - damage)
  def dead = health <= 0
}

case object PlayerGenerator {
  def generate = Being(Player, 10)
}

case object Player extends BeingDescriptor with Sighted {

  override def isThirdPerson = false
  val name = "you"
  val pronoun = "your"
  def drop = None

  private val viewportRange = 6
  val lineOfLightRange = Math.ceil(Math.sqrt(2 * Math.pow(viewportRange, 2)))

  def viewport(position: Position) = Area(
    Position(position.x - viewportRange + 1, position.y - viewportRange + 1),
    Position(position.x + viewportRange - 1, position.y + viewportRange - 1)
  )
}
