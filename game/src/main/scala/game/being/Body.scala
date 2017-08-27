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
case class BodyFellUnconscious(damage: Damage) extends BodyEffect

case class Body(fullHealth: Health, health: Health) extends Conscious with Mortal with Damagable {

  def dead = destroyed

  def unconscious: Boolean = damagePercentage < 0.3

  def struckBy(damage: Damage) = {
    val newBody = this.copy(health = health - damage)
    if (!this.destroyed && newBody.destroyed)
      unit(newBody, Some(BodyDestroyed(damage)))
    else if (!this.destroyed && newBody.destroyed)
      unit(newBody, Some(BodyFellUnconscious(damage)))
    else
      unit(newBody, Some(BodyDamaged(damage)))
  }
}

object HumanoidBody {
  def apply(health: Health): Body = Body(health, health)
}