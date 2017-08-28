package game


import dungeon.{Cell, Dungeon}
import dungeon.generation.DungeonGenerator
import dungeon.generation.DungeonGenerator.GenerationError
import dungeon.generation.floorplan.{BSPTree, Floorplan, RandomBSPTreeParameters}
import game.being.{Being, Player}
import math.{Direction, Position, Size}
import primitives.Ratio
import random.RNG
import random.RNG._
import com.softwaremill.quicklens._


case class ClosedInterval(min: Int, max: Int)

sealed trait Outcome
sealed trait CertainOutcome extends Outcome
sealed trait RandomOutcome extends Outcome {
  def outcomes: Rand[Set[CertainOutcome]]
}

case class Moved(direction: Direction) extends CertainOutcome
case class Drank(potion: Potion) extends CertainOutcome
case class DoorOpened(target: Position) extends CertainOutcome
case class LinearDamage(target: Position, valueRange: ClosedInterval) extends RandomOutcome {
  def outcomes: Rand[Set[CertainOutcome]] = {
    val delta = valueRange.max - valueRange.min
    RNG.nextPositiveInt(delta)
      .map(_ + valueRange.min)
      .map(i => Set(Damaged(target, i)))
  }
}
case class Damaged(target: Position, value: Int) extends CertainOutcome

sealed trait ActionTarget {
  def possibleOutcomes: Set[Outcome]
  def outcomes: Rand[Set[CertainOutcome]] = {
    val randomOutcomes = possibleOutcomes.toList.flatMap {
      case outcome: CertainOutcome => List(RNG.unit(Set(outcome)))
      case outcome: RandomOutcome => List(outcome.outcomes)
    }
    RNG.sequence(randomOutcomes).map(_.flatten.toSet)
  }
}
case class StrikeBeing(damageRange: ClosedInterval, target: Position) extends ActionTarget {
  def possibleOutcomes: Set[Outcome] = Set(LinearDamage(target, damageRange))
}
case class Move(direction: Direction) extends ActionTarget {
  def possibleOutcomes: Set[Outcome] = Set(Moved(direction))
}
case class UseItem(item: Item) extends ActionTarget {
  def possibleOutcomes: Set[Outcome] = item match {
    case Gold => Set()
    case potion: Potion => Set(Drank(potion))
  }
}
case class OpenDoor(target: Position) extends ActionTarget {
  def possibleOutcomes: Set[Outcome] = Set(DoorOpened(target))
}

case class GameState(dungeon: Dungeon, revealedPositions: Set[Position], notificationHistory: List[Notification]) {


  private def actionTargetAtDirection(sourcePosition: Position, direction: Direction): Set[ActionTarget] = {
    val sourceBeing = dungeon.cells(sourcePosition).being.get
    val targetPosition = sourcePosition.towards(direction, 1)
    dungeon.cells.get(targetPosition) match {
      case Some(Cell(Some(_), _, _)) =>
        Set(StrikeBeing(sourceBeing.descriptor.damageRange, targetPosition))
      case Some(Cell(None, Some(_:Openable), _)) =>
        Set(OpenDoor(targetPosition))
      case Some(cell@Cell(_, _, _)) if cell.passable =>
        Set(Move(direction))
      case _ =>
        Set()
    }
  }

  def actionTargetFromCommand(sourcePosition: Position, command: Command): Option[ActionTarget] = command match {
    case command: DirectionalCommand => actionTargetAtDirection(sourcePosition, command.direction) match {
      case set if set.isEmpty => None
      case nonEmptySet => Some(nonEmptySet.head)
    }
    case Command.Use(itemSlug) =>
      dungeon.cells(sourcePosition).being.get.itemBag.get(itemSlug).map(UseItem.apply)
  }

  def actionTargets(sourcePosition: Position): Set[ActionTarget] = {
    val sourceBeing = dungeon.cells(sourcePosition).being.get
    val itemActionTargets = sourceBeing.itemBag.items.keys.toSet.map(UseItem.apply)
    val directionActionTargets = Direction.all.flatMap(actionTargetAtDirection(sourcePosition, _))

    itemActionTargets ++ directionActionTargets
  }

  def materialize(sourcePosition: Position, certainOutcome: CertainOutcome): GameState = {
    val sourceBeing = dungeon.cells(sourcePosition).being.get
    certainOutcome match {

      case Moved(direction) =>
        val destinationPosition = sourcePosition.towards(direction, 1)
        val targetItemBag = dungeon.cells.get(destinationPosition).get.itemBag
        this
          .modify(_.dungeon.cells.at(sourcePosition).being)
          .setTo(None)
          .modify(_.dungeon.cells.at(destinationPosition).being)
          .setTo(Some(sourceBeing.modify(_.itemBag).using(_ + targetItemBag)))
          .modify(_.dungeon.cells.at(destinationPosition).itemBag)
          .setTo(ItemBag.empty)
          .modify(_.notificationHistory)
          .using(targetItemBag.items.map {
            case (item, amount) => TargetTaken(sourceBeing.descriptor, amount, item)
          }.toList ++: _)

      case Drank(potion) =>
        val drinkNotification = PotionDrank(sourceBeing.descriptor, potion)
        val effectNotification = BeingAffected(sourceBeing.descriptor, potion.effect)
        this
          .modify(_.dungeon.cells.at(sourcePosition).being.each)
          .setTo(potion.effect match {
            case FullyHeal => sourceBeing.modify(_.body.health).setTo(sourceBeing.body.fullHealth)
          })
          .modify(_.dungeon.cells.at(sourcePosition).being.each.itemBag)
          .using(_ - potion)
          .modify(_.notificationHistory)
          .using(effectNotification :: drinkNotification :: _)

      case DoorOpened(target) =>
        dungeon.cells(target).structure.get match {
          case openable: Openable => this
            .modify(_.dungeon.cells.at(target).structure).setTo(Some(openable.opened))
            .modify(_.notificationHistory).using(TargetOpened(sourceBeing.descriptor, openable) +: _)
          case _ => this
        }

      case Damaged(target, damage) =>
        val targetBeing = dungeon.cells.get(target).get.being.get
          .modify(_.body.health.value).using(_ - damage)

        def hitNotifications = List(TargetHit(sourceBeing.descriptor, targetBeing.descriptor, damage))
        def deathNotifications =
          if(targetBeing.body.dead)
            TargetDies(targetBeing.descriptor) :: targetBeing.itemBag.items.map {
              case (item, amount) => TargetDropsItem(targetBeing.descriptor, amount, item)
            }.toList
          else List()

        (
          if (targetBeing.body.dead)
            this
              .modify(_.dungeon.cells.at(target).being).setTo(None)
              .modify(_.dungeon.cells.at(target).itemBag).using(_ + targetBeing.itemBag)
          else
            this
              .modify(_.dungeon.cells.at(target).being).setTo(Some(targetBeing))
          )
          .modify(_.notificationHistory).using((deathNotifications ++ hitNotifications) ++: _)
    }
  }

}


sealed trait Command
trait DirectionalCommand extends Command {
  def direction: Direction
}

object Command {
  case object Up extends DirectionalCommand {
    val direction = Direction.Up
  }
  case object Down extends DirectionalCommand {
    val direction = Direction.Down
  }
  case object Right extends DirectionalCommand {
    val direction = Direction.Right
  }
  case object Left extends DirectionalCommand {
    val direction = Direction.Left
  }
  case class Use(itemSlug: ItemSlug) extends Command

  def fromKeyCode(keyCode: Int): Option[Command] = keyCode match {
    case 37 => Some(Command.Left)
    case 38 => Some(Command.Up)
    case 39 => Some(Command.Right)
    case 40 => Some(Command.Down)
    case _  => None
  }
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
      case (Right(dungeon), newRng) => dungeon.playerPosition match {

        case Some(position) =>(
          GameState(
            dungeon = dungeon,
            revealedPositions = Player.positionsWithinRangeTouchedByPerimeterRay(position, dungeon),
            notificationHistory = List()
          ),
          newRng
          )

        case None => throw new Exception("Dungeon needs a player")
      }
      case (Left(error), _) => throw new Exception("Dungeon generation failed")
    }

  }
}
