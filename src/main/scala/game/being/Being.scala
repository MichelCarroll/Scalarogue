package game.being

import dungeon.Dungeon
import game.being.ai.Intelligence
import game.combat.{Damage, DamageRange, Health}
import game.{Command, Gold, Items}
import math.{Area, Position}
import random.RNG._


case class Being(descriptor: BeingDescriptor, health: Health, intelligence: Intelligence) {

  def hit(damage: Damage) = Being(descriptor, health - damage, intelligence)

  def dead = health.value <= 0

  def withNextCommand(position: Position, dungeon: Dungeon): Rand[(Option[Command], Being)] =
    intelligence.nextCommand(PositionedBeing(position, this), dungeon).map {
      case (nextCommand, nextIntelligence) => (nextCommand, Being(descriptor, health, nextIntelligence))
    }
  
}

case class PositionedBeing(position: Position, being: Being)

sealed trait BeingDescriptor {
  def name: String
  def pronoun: String
  def drop: Option[Items]
  def isThirdPerson = true
  def damageRange: DamageRange
  def maxHealth: Health
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


case object Spider extends BeingDescriptor {
  def name = "a spider"
  def pronoun = "it"
  def drop = Some(Gold(5))
  def damageRange = DamageRange(Damage(1), Damage(2))
  def maxHealth = Health(5)
}