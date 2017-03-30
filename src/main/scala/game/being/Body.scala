package game.being

import random.RNG
import random.RNG._


case class Body(health: Health) {
  def damagedBy(damage: Damage) = Body(health - damage)
}

trait BodyFactory {
  def randomNewBody: Rand[Body]
}

class SimpleGaussianBodyFactory(meanHealth: Int, variation: Int) extends BodyFactory {
  def randomNewBody: Rand[Body] = RNG.nextGaussianRatio(3)
    .map { ratio =>
      val randomHealthValue = (meanHealth - variation + variation * 2 * ratio).round.toInt
      Body(Health(Math.max(1, randomHealthValue)))
    }
}