package game.being

import random.RNG._


case class DamageRange(min: Damage, max: Damage) {
  def delta = max.value - min.value
  def randomDamage: Rand[Damage] = nextPositiveInt(delta) map (Damage(_) + min)
}

case class Damage(value: Int) {
  def +(x: Damage) = Damage(value + x.value)
}

case class Health(value: Int) extends AnyVal {
  def +(x: Health) = Health(value + x.value)
  def -(x: Damage) = Health(value - x.value)
}