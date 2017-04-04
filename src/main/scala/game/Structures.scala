package game

sealed trait Structure extends Describable {
  def opaque: Boolean
}

sealed trait Openable extends Structure {
  def opened: Structure
}

sealed trait Blocking extends Structure

case object Upstairs extends Structure with Blocking {
  val name = "stairs"
  def opaque = false
}

case object Downstairs extends Structure {
  val name = "stairs"
  val opaque = false
}

case object OpenedDoor extends Structure {
  val name = "door"
  def opaque = false
}

case object ClosedDoor extends Structure with Openable with Blocking {
  val name = "door"
  def opaque = true
  def opened = OpenedDoor
}
