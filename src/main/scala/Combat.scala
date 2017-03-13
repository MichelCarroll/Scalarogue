import math.RNG
import math.RNG.Rand

case class Health(value: Int) extends AnyVal {
  def +(x: Health) = Health(value + x.value)
  def -(x: Damage) = Health(value - x.value)
}

case class Damage(value: Int) {
  def +(x: Damage) = Damage(value + x.value)
}

case class DamageRange(min: Damage, max: Damage) {

  def delta = max.value - min.value
  def randomDamage: Rand[Damage] = RNG.map(RNG.nextPositiveInt(delta))(Damage(_) + min)

}