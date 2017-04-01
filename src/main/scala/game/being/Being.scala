package game.being

import dungeon.Dungeon
import game.being.ai.Intelligence
import game.{Command, Notification, TargetGetsBodyEffect}
import math.Position
import random.RNG._


case class Being(descriptor: BeingDescriptor, body: Body, intelligence: Intelligence) {

  def hit(damage: Damage): Rand[(Being, Option[Notification])] =
    body.struckBy(damage)
      .map { case (newBody, bodyEffectOpt) => (
        copy(body = newBody),
        bodyEffectOpt.map(TargetGetsBodyEffect(descriptor, _))
      )}

  def withNextCommand(position: Position, dungeon: Dungeon): Rand[(Option[Command], Being)] =
    intelligence.nextCommand(PositionedBeing(position, this), dungeon).map {

      case (nextCommand, nextIntelligence) =>  {
        (nextCommand, Being(descriptor, body, nextIntelligence))
      }
    }

}

case class PositionedBeing(position: Position, being: Being)
