

sealed trait Being
case object Nugget extends Being

sealed trait Structure {
  def passable: Boolean
  def opaque: Boolean
}
object Structure {
  case object ClosedDoor extends Structure {
    def passable = false
    def opaque = true
  }
  case object OpenedDoor extends Structure {
    def passable = true
    def opaque = false
  }
  case object Upstairs extends Structure {
    def passable = true
    def opaque = false
  }
  case object Downstairs extends Structure {
    def passable = true
    def opaque = false
  }
}

trait Item

sealed trait Cell {
  def passable: Boolean
  def opaque: Boolean
}
case class OpenCell(being: Option[Being] = None, structure: Option[Structure] = None, item: Option[Item] = None) extends Cell {
  def passable = being.isEmpty && structure.forall(_.passable)
  def opaque = structure.exists(_.opaque)
}
case object ClosedCell extends Cell {
  def passable = false
  def opaque = true
}
