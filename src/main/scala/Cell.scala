

sealed trait Being
case object Nugget extends Being

sealed trait Structure {
  def passable: Boolean
}
object Structure {
  case object ClosedDoor extends Structure {
//    def passable = false
def passable = true
  }
  case object OpenedDoor extends Structure {
    def passable = true
  }
  case object Upstairs extends Structure {
    def passable = true
  }
  case object Downstairs extends Structure {
    def passable = true
  }
}

trait Item

sealed trait Cell {
  def passable: Boolean
}
case class OpenCell(being: Option[Being] = None, structure: Option[Structure] = None, item: Option[Item] = None) extends Cell {
  def passable = being.isEmpty && structure.forall(_.passable)
}
case object ClosedCell extends Cell {
  def passable = false
}
