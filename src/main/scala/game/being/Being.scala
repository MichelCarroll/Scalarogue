package game.being

import dungeon.Dungeon
import game.being.ai.{Intelligence, IntelligenceFactory}
import game.{Command, Items}
import math.Position
import random.RNG._


case class Being(descriptor: BeingDescriptor, body: Body, intelligence: Intelligence) {

  def hit(damage: Damage) = Being(descriptor, body damagedBy damage, intelligence)

  def dead = body.health.value <= 0

  def withNextCommand(position: Position, dungeon: Dungeon): Rand[(Option[Command], Being)] =
    intelligence.nextCommand(PositionedBeing(position, this), dungeon).map {

      case (nextCommand, nextIntelligence) =>  {
        (nextCommand, Being(descriptor, body, nextIntelligence))
      }
    }

}

case class PositionedBeing(position: Position, being: Being)
