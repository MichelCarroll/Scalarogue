package game.being

import dungeon.Dungeon
import game.being.ai.Intelligence
import game.{Command, Notification}
import math.Position
import random.RNG._


case class Being(descriptor: BeingDescriptor, body: Body, intelligence: Intelligence) {

  def hit(damage: Damage): Rand[(Being, Option[Notification])] =
    body.struckBy(damage)
      .map { case (newBody, notificationOpt) => (copy(body = newBody), notificationOpt) }

  def withNextCommand(position: Position, dungeon: Dungeon): Rand[(Option[Command], Being)] =
    intelligence.nextCommand(PositionedBeing(position, this), dungeon).map {

      case (nextCommand, nextIntelligence) =>  {
        (nextCommand, Being(descriptor, body, nextIntelligence))
      }
    }

}

case class PositionedBeing(position: Position, being: Being)
