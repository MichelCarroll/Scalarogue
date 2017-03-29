package game.combat

import random.RNG
import random.RNG._

case class DamageRange(min: Damage, max: Damage) {

  def delta = max.value - min.value
  def randomDamage: Rand[Damage] = RNG.map(RNG.nextPositiveInt(delta))(Damage(_) + min)

}
