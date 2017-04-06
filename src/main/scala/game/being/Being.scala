package game.being

import dungeon.Dungeon
import game.being.ai.Intelligence
import game.Command
import math.Position
import random.RNG._


case class Being(descriptor: BeingDescriptor, body: Body, intelligence: Intelligence) {

  def hit(damage: Damage): Rand[(Being, Option[BodyEffect])] =
    body.struckBy(damage)
      .map { case (newBody, bodyEffectOpt) => (copy(body = newBody), bodyEffectOpt)}

}

case class PositionedBeing(position: Position, being: Being)
