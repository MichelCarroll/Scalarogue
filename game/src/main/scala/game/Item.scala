package game

/**
  * Created by MichelCarroll on 3/28/2017.
  */
sealed trait Item {
  val name: String
  val capitalizedName: String
}

/**
  * Created by MichelCarroll on 3/28/2017.
  */
case object Gold extends Item {
  val name = "gold"
  val capitalizedName = "Gold"
}

case object HealthPotion extends Item {
  val name = "health potion"
  val capitalizedName = "Health Potion"
}

case class ItemBag(items: Map[Item, Int]) extends AnyVal {

  //not super optimized. would have been better with a Multiset, but its not ScalaJS compatible
  def +(other: ItemBag): ItemBag = {
    val mergedItems = (items.toSeq ++ other.items.toSeq)
      .groupBy(_._1).mapValues(_.map(_._2).sum)
    ItemBag(mergedItems)
  }

}

object ItemBag {
  val empty = ItemBag(Map[Item, Int]())

  def apply(items: (Item, Int)*): ItemBag = ItemBag(items.toMap)
}