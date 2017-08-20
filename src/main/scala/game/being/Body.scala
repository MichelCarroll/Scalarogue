package game.being

import random.RNG
import random.RNG._


trait Mortal {
  def dead: Boolean
}

trait Conscious {
  def unconscious: Boolean
}

trait Damagable {
  val fullHealth: Health
  val health: Health
  val destroyed = health.value <= 0
  def damagePercentage: Double = health / fullHealth
}

trait Body extends Conscious with Mortal with Damagable {
  def struckBy(damage: Damage): Rand[(Body, Option[BodyEffect])]
}

trait BodyFactory {
  def randomNewBody: Rand[Body]
}

class SimpleHumanoidGaussianBodyFactory(meanHealth: Int, variation: Int) extends BodyFactory {

  def randomNewBody: Rand[Body] = {

    def randomHealth = RNG.nextGaussianRatio(3)
      .map(ratio => Health((meanHealth - variation + variation * 2 * ratio).round.toInt))

    randomHealth.map(HumanoidBody.apply)
  }
}

sealed trait BodyEffect
case class BodyDamaged(damage: Damage) extends BodyEffect
case class BodyDestroyed(damage: Damage) extends BodyEffect

case class HumanoidBody(fullHealth: Health, health: Health) extends Body {

  def dead = destroyed

  def unconscious: Boolean = damagePercentage < 0.3

  def struckBy(damage: Damage) = {
    val newBody = this.copy(health = health - damage)
    if (newBody.destroyed)
      unit(newBody, Some(BodyDamaged(damage)))
    else
      unit(newBody, Some(BodyDamaged(damage)))
  }
}

object HumanoidBody {
  def apply(health: Health): HumanoidBody = HumanoidBody(health, health)
}