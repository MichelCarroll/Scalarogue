package game.being

import game.{Gold, HealthPotion, Item, ItemBag}
import game.being.ai.Intelligence
import math.Position
import random.RNG._
import com.softwaremill.quicklens._

case class Being(descriptor: BeingDescriptor, body: Body, intelligence: Intelligence, itemBag: ItemBag) {

  def goldAmount = itemBag.items.map {
    case (Gold, i) => i
    case _ => 0
  }.sum

}