package game

sealed trait Structure extends Describable

sealed trait Openable extends Structure {
  def opened: Structure
}

sealed trait Blocking extends Structure
sealed trait Opaque extends Structure

case object Upstairs extends Structure with Blocking {
  val name = "stairs"
}

case object Downstairs extends Structure {
  val name = "stairs"
}

case object OpenedDoor extends Structure {
  val name = "door"
}

case object ClosedDoor extends Structure with Openable with Blocking with Opaque {
  val name = "door"
  def opened = OpenedDoor
}
