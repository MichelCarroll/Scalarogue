package game.being.ai

import dungeon.Dungeon
import game.being.PositionedBeing
import game.Command
import random.RNG._
import math._


trait Intelligence {
  def nextCommand(positionedBeing: PositionedBeing, dungeon: Dungeon): Rand[(Option[Command], Intelligence)]
}

case object NoIntelligence extends Intelligence {
  def nextCommand(positionedBeing: PositionedBeing, dungeon: Dungeon) = unit(None, this)
}

case object RandomIntelligence extends Intelligence {
  def nextCommand(positionedBeing: PositionedBeing, dungeon: Dungeon) =
    nextFromSet(Command.all).map(x => (x, this))
}

case class SimpleAgroIntelligence(maxRange: Int) extends Intelligence {
  def nextCommand(positionedBeing: PositionedBeing, dungeon: Dungeon) = {
    unit(
      dungeon.positionedPlayer.flatMap(player =>
        if(player.position.manhattanDistanceTo(positionedBeing.position) <= maxRange)
          dungeon.bestDirectionTo(positionedBeing.position, player.position).map {
            case Direction.Up => Command.Up
            case Direction.Down => Command.Down
            case Direction.Right => Command.Right
            case Direction.Left => Command.Left
          }
        else
          None
      )
      ,this
    )
  }
}

trait IntelligenceFactory {
  def randomNewIntelligence: Rand[Intelligence]
}