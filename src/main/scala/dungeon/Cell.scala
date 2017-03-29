package dungeon

import game.{Blocking, Items, Structure}
import game.being.Being
/**
  * Created by MichelCarroll on 3/28/2017.
  */
sealed trait Cell {
  def passable: Boolean
  def opaque: Boolean
}

/**
  * Created by MichelCarroll on 3/28/2017.
  */
case class OpenCell(
                     being: Option[Being] = None,
                     structure: Option[Structure] = None,
                     item: Set[Items] = Set()
                   ) extends Cell {
  def passable = being.isEmpty && !structure.exists {
    case _: Blocking => true
    case _ => false
  }
  def opaque = structure.exists(_.opaque)
}

/**
  * Created by MichelCarroll on 3/28/2017.
  */
case object ClosedCell extends Cell {
  def passable = false
  def opaque = true
}
