package dungeon

import game._
import game.being.Being


/**
  * Created by MichelCarroll on 3/28/2017.
  */
case class Cell(
                     being: Option[Being] = None,
                     structure: Option[Structure] = None,
                     itemBag: ItemBag = ItemBag.empty
                   ) {
  def passable = being.isEmpty && !structure.exists {
    case _: Blocking => true
    case _ => false
  }
  def opaque = structure.exists {
    case _: Opaque => true
    case _ => false
  }
}
