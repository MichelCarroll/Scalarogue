package game.being

import game.ClosedInterval
import random.RNG
import random.RNG._


trait Mortal {
  def dead: Boolean
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

    randomHealth.map(health => HumanoidBody(health, health))
  }
}

class SimpleArachnoidGaussianBodyFactory(meanHealth: Int, variation: Int) extends BodyFactory {

  def randomNewBody: Rand[Body] = {

    def randomHealth = RNG.nextGaussianRatio(3)
      .map(ratio => Health((meanHealth - variation + variation * 2 * ratio).round.toInt))

    randomHealth.map(health => ArachnoidBody(health, health))
  }
}

sealed trait Body extends Mortal with Damagable {
  val fullHealth: Health
  val health: Health
  def weaponlessDamageRange: ClosedInterval
}
case class HumanoidBody(fullHealth: Health, health: Health) extends Body {
  def dead = destroyed
  def weaponlessDamageRange = ClosedInterval(2, 4)
}
case class ArachnoidBody(fullHealth: Health, health: Health) extends Body {
  def dead = destroyed
  def weaponlessDamageRange = ClosedInterval(1, 2)
}