package game

case class ItemSlug(value: String) extends AnyVal

sealed trait BeingEffect {
  def description: String
}
case object FullyHeal extends BeingEffect {
  val description = "fully healed"
}

sealed trait Potion extends Item {
  def effect: BeingEffect
}

/**
  * Created by MichelCarroll on 3/28/2017.
  */
sealed trait Item {
  val slug: ItemSlug
  val name: String
  val capitalizedName: String
}

/**
  * Created by MichelCarroll on 3/28/2017.
  */
case object Gold extends Item {
  val slug = ItemSlug("gold")
  val name = "gold"
  val capitalizedName = "Gold"
}

case object HealthPotion extends Potion {
  val slug = ItemSlug("health-potion")
  val name = "health potion"
  val capitalizedName = "Health Potion"
  val effect = FullyHeal
}

case class ItemBag(items: Map[Item, Int]) extends AnyVal {

  //not super optimized. would have been better with a Multiset, but its not ScalaJS compatible
  def +(other: ItemBag): ItemBag = {
    val mergedItems = (items.toSeq ++ other.items.toSeq)
      .groupBy(_._1).mapValues(_.map(_._2).sum)
    ItemBag(mergedItems)
  }

  def get(itemSlug: ItemSlug): Option[Item] = items.find {
    case (item, amount) => item.slug == itemSlug && amount > 0
  }.map(_._1)

  def -(item: Item): ItemBag = items(item) match {
    case amount if amount == 1 => ItemBag(items - item)
    case amount if amount >= 2 => ItemBag(items.updated(item, amount - 1))
  }

}

object ItemBag {
  val empty = ItemBag(Map[Item, Int]())

  def apply(items: (Item, Int)*): ItemBag = ItemBag(items.toMap)
}