package game.being

import game.{Gold, Item, ItemBag}
import game.being.ai.Intelligence
import math.Position
import random.RNG._


case class Being(descriptor: BeingDescriptor, body: Body, intelligence: Intelligence, itemBag: ItemBag) {

  def hit(damage: Damage): Rand[(Being, Option[BodyEffect])] =
    body.struckBy(damage)
      .map { case (newBody, bodyEffectOpt) => (copy(body = newBody), bodyEffectOpt)}

  def goldAmount = itemBag.items.map {
    case (Gold, i) => i
    case _ => 0
  }.sum

}