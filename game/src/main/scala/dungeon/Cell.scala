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

  def description = {
    val beingDescriptionOpt = beingOpt.map(being => s"You see a ${being.descriptor.name}. ")
    val structureDescriptionOpt = structureOpt.map(structure => s"You see ${structure.name}. ")
    val itemDescriptionsOpt =
      if(itemBag.items.isEmpty) None
      else {
        val itemShortDescriptions = itemBag.items.toList.map { case (item, n) => s"$n ${item.name}" }
        Some(itemShortDescriptions.mkString("On the floor, you see ", ", ", "."))
      }

    (beingDescriptionOpt, structureDescriptionOpt, itemDescriptionsOpt) match {
      case (None, None, None) => "You see the floor."
      case _ =>
        List(beingDescriptionOpt, structureDescriptionOpt, itemDescriptionsOpt).flatten.mkString(" ")
    }
  }
}
