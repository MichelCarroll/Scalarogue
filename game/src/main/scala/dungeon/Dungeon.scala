package dungeon

import dungeon.pathfinding.Navigatable
import game.being.{Being, BeingDescriptor, Player}
import math._

/**
  * Created by MichelCarroll on 3/28/2017.
  */
case class Dungeon(cells: Map[Position, Cell], area: Area, entrancePosition: Position) extends Navigatable {

  def update(position: Position, being: Being) =  copy(
    cells = cells.get(position) match {
      case Some(o@OpenCell(_,_,_)) => cells.updated(position, o.copy(being = Some(being)))
      case _ => throw new Exception("You cannot update a closed cell with a being")
    }
  )

  def withUpdatedCell(at: Position, cell: Cell) = copy(
    cells = cells.updated(at, cell)
  )

  def withRemovedBeing(at: Position) = copy(
    cells = cells.get(at) match {
      case Some(OpenCell(_, structure, items)) => cells.updated(at, OpenCell(None, structure, items))
      case _ => cells
    }
  )

  def playerPosition: Option[Position] = cells
    .map {
      case (position, OpenCell(Some(Being(Player, _, _, _)), _, _)) => Some(position)
      case _ => None
    }
    .find(_.isDefined)
    .flatten

  def player: Option[Being] = cells
    .map {
      case (position, OpenCell(Some(being@Being(Player, _, _, _)), _, _)) => Some(being)
      case _ => None
    }
    .find(_.isDefined)
    .flatten

  def beingOfTypePositions(target: BeingDescriptor) = cells
    .flatMap {
      case (position, OpenCell(Some(being@Being(descriptor, _, _, _)), _, _)) if descriptor == target => Some(position)
      case _ => None
    }
    .toSet


}