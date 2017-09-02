package game

import dungeon.Cell
import linguistics.Sentence


trait Named { self =>
  val name: String

  def ownedBy(o: Describable): Describable = new Describable {
    override val isProtagonist = false
    override val owner = Some(o)
    override val name = self.name
  }
}

trait Describable extends Named {
  val isProtagonist: Boolean = false
  val owner: Option[Describable] = None

  def subject = owner match {
    case Some(o) if o.isProtagonist => s"your $name"
    case Some(o) => s"the ${o.name}'s $name"
    case None if isProtagonist => s"you"
    case None => s"the $name"
  }

  def subjectUsingPronouns = owner match {
    case Some(o) if o.isProtagonist => s"your $name"
    case Some(o) => s"his $name"
    case None if isProtagonist => s"you"
    case None => s"he"
  }
}

trait Notification extends Sentence {
  val message: String
}

case object LookAtDarkness extends Notification {
  val message = s"you look into the dismal abyss"
}

case class LookAtCell(cell: Cell) extends Notification {

  val message = {
    val beingDescriptionOpt = cell.beingOpt.map(being => s"you see the ${being.descriptor.name}")
    val structureDescriptionOpt = cell.structureOpt.map(structure => s"you see ${structure.name}")
    val itemDescriptionsOpt =
      if(cell.itemBag.items.isEmpty) None
      else {
        val itemShortDescriptions = cell.itemBag.items.toList.map { case (item, n) => numbered(item, n) }
        Some(itemShortDescriptions.mkString("on the floor, you see ", ", ", ""))
      }

    (beingDescriptionOpt, structureDescriptionOpt, itemDescriptionsOpt) match {
      case (None, None, None) => "you see the floor"
      case _ =>
        List(beingDescriptionOpt, structureDescriptionOpt, itemDescriptionsOpt).flatten.mkString(", and ")
    }
  }

}

case class ItemStash(by: Describable, item: Item) extends Notification {
  val message = s"${by.subject} stashed ${numbered(item, 1)}"
}

case class ItemHeld(by: Describable, item: Item) extends Notification {
  val message = s"${by.subject} held ${numbered(item, 1)}"
}

case class PotionDrank(by: Describable, potion: Potion) extends Notification {
  val message = s"${by.subject} drank ${numbered(potion, 1)}"
}

case class BeingAffected(by: Describable, beingEffect: BeingEffect) extends Notification {
  val message = s"${by.subject} were ${beingEffect.description}"
}

case class TargetTaken(by: Describable, amount: Int, item: Item) extends Notification {
  val message = s"${by.subject} picked up and stashed ${numbered(item, amount)}"
}

case class TargetOpened(by: Describable, target: Describable) extends Notification {
  val message = s"${by.subject} opened ${target.subject}"
}

case class TargetHit(by: Describable, target: Describable, damage: Int) extends Notification {
  val message = s"${by.subject} hit ${target.subject}, ${target.subjectUsingPronouns} received $damage damage"
}

case class TargetDies(target: Describable) extends Notification {
  val message = s"${target.subject} died!"
}

case class TargetDropsItem(by: Describable, amount: Int, item: Item) extends Notification {
  val message = s"${by.subject} dropped ${numbered(item, amount)}!"
}