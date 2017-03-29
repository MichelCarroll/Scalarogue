package game.combat

/**
  * Created by MichelCarroll on 3/28/2017.
  */
case class Health(value: Int) extends AnyVal {
  def +(x: Health) = Health(value + x.value)
  def -(x: Damage) = Health(value - x.value)
}
