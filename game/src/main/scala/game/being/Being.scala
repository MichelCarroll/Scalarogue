package game.being

import game.{Gold, Item}
import game.being.ai.Intelligence
import math.Position
import random.RNG._


case class Being(descriptor: BeingDescriptor, body: Body, intelligence: Intelligence, items: Set[Item]) {

  def hit(damage: Damage): Rand[(Being, Option[BodyEffect])] =
    body.struckBy(damage)
      .map { case (newBody, bodyEffectOpt) => (copy(body = newBody), bodyEffectOpt)}

  def goldAmount = items.map {
    case Gold(amount) => amount
    case _ => 0
  }.sum

}