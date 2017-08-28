package game.being



case class Health(value: Int) extends AnyVal {
  def +(x: Health) = Health(value + x.value)
  def /(x: Health): Double = value.toDouble / x.value.toDouble
}