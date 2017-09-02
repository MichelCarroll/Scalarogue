package dungeon

import game._
import game.being.Being


/**
  * Created by MichelCarroll on 3/28/2017.
  */
case class Cell(
                 beingOpt: Option[Being] = None,
                 structureOpt: Option[Structure] = None,
                 itemBag: ItemBag = ItemBag.empty
                   )  {
  def passable = beingOpt.isEmpty && !structureOpt.exists {
    case _: Blocking => true
    case _ => false
  }
  def opaque = structureOpt.exists {
    case _: Opaque => true
    case _ => false
  }
}
