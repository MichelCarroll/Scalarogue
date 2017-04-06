package game.being.ai

import dungeon.Dungeon
import game.Command
import random.RNG._
import math._


trait Intelligence {
  def nextCommand(fromPosition: Position, dungeon: Dungeon): Rand[Option[Command]]
}

case object NoIntelligence extends Intelligence {
  def nextCommand(fromPosition: Position, dungeon: Dungeon) = unit(None)
}

case object RandomIntelligence extends Intelligence {
  def nextCommand(fromPosition: Position, dungeon: Dungeon) = nextFromSet(Command.all)
}

case class SimpleAgroIntelligence(maxRange: Int) extends Intelligence {
  def nextCommand(fromPosition: Position, dungeon: Dungeon) = {
    unit(
      dungeon.positionedPlayer.flatMap(player =>
        if(player.position.manhattanDistanceTo(fromPosition) <= maxRange)
          dungeon.bestDirectionTo(fromPosition, player.position).map {
            case Direction.Up => Command.Up
            case Direction.Down => Command.Down
            case Direction.Right => Command.Right
            case Direction.Left => Command.Left
          }
        else
          None
      )
    )
  }
}

trait IntelligenceFactory {
  def randomNewIntelligence: Rand[Intelligence]
}