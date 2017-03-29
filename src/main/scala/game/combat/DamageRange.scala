package game.combat

import random.RNG._

case class DamageRange(min: Damage, max: Damage) {

  def delta = max.value - min.value
  def randomDamage: Rand[Damage] = nextPositiveInt(delta) map (Damage(_) + min)

}
