package game


import dungeon.{Cell, Dungeon, OpenCell}
import dungeon.generation.DungeonGenerator
import dungeon.generation.DungeonGenerator.GenerationError
import dungeon.generation.floorplan.{BSPTree, Floorplan, RandomBSPTreeParameters}
import game.RelationshipWithUser.Other
import game.being.{Being, PositionedBeing}
import math.{Position, Size}
import primitives.Ratio
import random.RNG
import random.RNG._


trait GameTransition {
  def newState: GameState
  def notifications: List[Notification]
}

case class IdentityTransition(state: GameState) extends GameTransition {
  def newState = state
  def notifications = List()
}

case class MoveTransition(subject: Being, oldCellPosition: Position, oldCell: OpenCell, newCellPosition: Position, newCell: OpenCell, items: Set[Item], state: GameState) extends GameTransition  {

  def notifications = items.map(item => TargetTaken(subject.descriptor, item)).toList

  def newState =  state.copy(
    dungeon = state.dungeon
      .withRemovedBeing(oldCellPosition)
      .withUpdatedCell(newCellPosition, newCell)
  )

}

case class OpenTransition(subject: Being, openable: Openable, newCell: Cell, position: Position, state: GameState) extends GameTransition  {
  def notifications = List(TargetOpened(subject.descriptor, openable))
  def newState = state.copy(dungeon = state.dungeon.withUpdatedCell(position, newCell))
}

case class HitTransition(sourceBeing: Being, targetBeing: Being, targetCell: OpenCell, targetBeingPosition: Position, state: GameState) extends GameTransition  {

  val (((newBeing, notificationOpt), damage), newRng) = sourceBeing.descriptor.damageRange
    .randomDamage
    .flatMap(damage => targetBeing.hit(damage).map((_, damage)))(state.rng)

  def damageNotifications = TargetHit(sourceBeing.descriptor, newBeing.descriptor, damage)
  def damageEffectNotifications = notificationOpt.map(List(_)).getOrElse(List())
  def deathNotifications =
    if(newBeing.body.dead)
      newBeing.descriptor.drop match {
        case Some(item) => List(TargetDies(newBeing.descriptor), TargetDropsItem(targetBeing.descriptor, item))
        case None => List(TargetDies(newBeing.descriptor))
      }
    else List()

  def notifications = damageNotifications :: damageEffectNotifications ++ deathNotifications

  def newState = {
    val updatedDungeon = if(newBeing.body.dead)
      state.dungeon.withUpdatedCell(targetBeingPosition,
        OpenCell(None, targetCell.structure, newBeing.descriptor.drop.map(targetCell.item + _).getOrElse(targetCell.item))
      )
    else
      state.dungeon.withUpdatedCell(targetBeingPosition, OpenCell(Some(newBeing), targetCell.structure, targetCell.item))

    state.copy(dungeon = updatedDungeon, rng = newRng)
  }

}

case class GameState(dungeon: Dungeon, rng: RNG) {
  import Command._

  def applyCommand(positionedBeing: PositionedBeing, command: Command): GameTransition = {

    def attemptNewPosition(position: Position): GameTransition =
      dungeon.cells.get(position) match {

        case Some(cell@OpenCell(Some(being: Being), structure, itemsOnGround)) =>
          HitTransition(positionedBeing.being, being, cell, position, this)

        case Some(OpenCell(None, Some(openable: Openable), items)) =>
          OpenTransition(positionedBeing.being, openable, OpenCell(None, Some(openable.opened), items), position, this)

        case Some(cell@OpenCell(None, structure, items)) if cell.passable =>
          MoveTransition(positionedBeing.being, positionedBeing.position, cell, position, OpenCell(Some(positionedBeing.being), structure, Set()), items, this)

        case _ =>
          IdentityTransition(this)

      }

    command match {
      case Up => attemptNewPosition(positionedBeing.position.up(1))
      case Down => attemptNewPosition(positionedBeing.position.down(1))
      case Left => attemptNewPosition(positionedBeing.position.left(1))
      case Right => attemptNewPosition(positionedBeing.position.right(1))
    }
  }

}





sealed trait Command
object Command {
  case object Up extends Command
  case object Down extends Command
  case object Right extends Command
  case object Left extends Command

  def fromKeyCode(keyCode: Int): Option[Command] = keyCode match {
    case 37 => Some(Command.Left)
    case 38 => Some(Command.Up)
    case 39 => Some(Command.Right)
    case 40 => Some(Command.Down)
    case _  => None
  }

  def all: Set[Command] = Set(Up, Down, Right, Left)
}


/**
  * Created by MichelCarroll on 3/28/2017.
  */
object GameState {

  def generatedDungeon: Rand[Either[GenerationError, Dungeon]] = {
    val gridSize = Size(50, 50)
    for {
      randomTree <- BSPTree.generate(
        RandomBSPTreeParameters(
          size = gridSize,
          minLeafEdgeLength = 3,
          minLeafSurfaceRelativeToTotal = Ratio(0.2)
        )
      )
      floorplan <- Floorplan.generate(randomTree, gridSize)
      dungeonEither <- DungeonGenerator.generate(floorplan)
    } yield dungeonEither
  }

  def start: Rand[GameState] = rng => {

    generatedDungeon(rng) match {
      case (Right(dungeon), newRng) =>
        (
          GameState(
            dungeon = dungeon,
            rng = newRng
          ),
          newRng
        )
      case (Left(error), _) => throw new Exception("Dungeon generation failed")
    }

  }
}
