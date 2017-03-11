
sealed trait Item {
  val amount: Int
  val name: String
}

sealed trait Openable extends Structure {
  def opened: Structure
}

sealed trait Blocking extends Structure

sealed trait Structure {
  def opaque: Boolean
}

sealed trait Cell {
  def passable: Boolean
  def opaque: Boolean
}


case class Gold(amount: Int) extends Item {
  val name = "gold"
}

case object ClosedDoor extends Structure with Openable with Blocking {
  def opaque = true
  def opened = OpenedDoor
}
case object OpenedDoor extends Structure {
  def opaque = false
}
case object Upstairs extends Structure with Blocking {
  def opaque = false
}
case object Downstairs extends Structure {
  def opaque = false
}

case class OpenCell(
                     being: Option[Being] = None,
                     structure: Option[Structure] = None,
                     item: Set[Item] = Set()
                   ) extends Cell {
  def passable = being.isEmpty && !structure.exists {
    case _: Blocking => true
    case _ => false
  }
  def opaque = structure.exists(_.opaque)
}
case object ClosedCell extends Cell {
  def passable = false
  def opaque = true
}
