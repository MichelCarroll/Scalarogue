package game.being.ai

import dungeon.Dungeon
import game.being.PositionedBeing
import game.Command
import random.RNG._
import math._

/**
  * Created by MichelCarroll on 3/28/2017.
  */
trait Intelligence {
  def nextCommand(positionedBeing: PositionedBeing, dungeon: Dungeon): Rand[(Option[Command], Intelligence)]
}

case class NoIntelligence() extends Intelligence {
  def nextCommand(positionedBeing: PositionedBeing, dungeon: Dungeon) = unit(None, this)
}

case class RandomIntelligence() extends Intelligence {
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