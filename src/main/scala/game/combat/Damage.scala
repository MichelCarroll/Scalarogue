package game.combat

/**
  * Created by MichelCarroll on 3/28/2017.
  */
case class Damage(value: Int) {
  def +(x: Damage) = Damage(value + x.value)
}
