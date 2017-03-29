package game

sealed trait Structure {
  def opaque: Boolean
}

sealed trait Openable extends Structure {
  def opened: Structure
}

sealed trait Blocking extends Structure

case object Upstairs extends Structure with Blocking {
  def opaque = false
}

case object Downstairs extends Structure {
  def opaque = false
}

case object OpenedDoor extends Structure {
  def opaque = false
}

case object ClosedDoor extends Structure with Openable with Blocking {
  def opaque = true
  def opened = OpenedDoor
}
