package game


import dungeon.{Cell, Dungeon}
import dungeon.generation.DungeonGenerator
import dungeon.generation.DungeonGenerator.GenerationError
import dungeon.generation.floorplan.{BSPTree, Floorplan, RandomBSPTreeParameters}
import game.being.{Being, Handed, Player}
import math.{Direction, Position, Size}
import primitives.Ratio
import random.RNG._
import com.softwaremill.quicklens._
import game.Command.Use


case class ClosedInterval(min: Int, max: Int) {
  def values = min to max
}

sealed trait Event
case class Moved(source: Position, direction: Direction) extends Event
case class Stashed(source: Position, item: Item) extends Event
case class Held(source: Position, item: Item) extends Event
case class PickedUp(source: Position, amount: Int, item: Item) extends Event
case class Dropped(source: Position, amount: Int, item: Item) extends Event
case class PotionDrinked(source: Position, potion: Potion) extends Event
case class DoorOpened(source: Position, target: Position) extends Event
case class Damaged(source: Position, target: Position, value: Int) extends Event
case class Died(source: Position) extends Event

case class GameState(dungeon: Dungeon, revealedPositions: Set[Position], notificationHistory: List[Notification]) {

  type TurnAmount = Int
  type WeightedOutcome = (Int, List[Event])
  type ActionTarget = Set[WeightedOutcome]

  def withUpdatedRevealedPositions: GameState =
    this.dungeon.playerPosition match {
      case Some(position) => this
          .modify(_.revealedPositions)
          .using(_ ++ Player.positionsWithinRangeTouchedByPerimeterRay(position, dungeon))
      case _ => this
    }

  def actionTargetMapping(source: Position): Map[Command, ActionTarget] = {

    def certainOutcome(events: List[Event]) = Set(1 -> events)

    def actionTargetAtDirection(direction: Direction): Option[ActionTarget] = {
      val sourceBeing = dungeon.cells(source).being.get
      val target = source.towards(direction, 1)
      dungeon.cells.get(target) match {

        case Some(Cell(Some(targetBeing), _, _)) =>
          Some(
            sourceBeing.body.damageRange.values.toSet.map((damage:Int) =>
              if(targetBeing.body.modify(_.health.value).using(_ - damage).destroyed)
                1 -> (Damaged(source, target, damage) :: targetBeing.itemBag.items.toList.map {
                  case (item, n) => Dropped(target, n, item)
                } ++ List(Died(target)))
              else
                1 -> List(Damaged(source, target, damage))
            )
          )

        case Some(Cell(None, Some(_:Openable), _)) =>
          Some(certainOutcome(List(DoorOpened(source, target))))

        case Some(cell@Cell(_, _, itemBag)) if cell.passable =>
          val pickUps = itemBag.items.map { case (item, n) => PickedUp(target, n, item) }.toList
          Some(certainOutcome(Moved(source, direction) :: pickUps))

        case _ => None
      }
    }

    val sourceBeing = dungeon.cells(source).being.get

    val itemsActionTargets: Map[Command, ActionTarget] = sourceBeing.itemBag.items.keys.flatMap { item =>
      val mappings: Set[(Command, ActionTarget)] = item match {
        case potion: Potion =>
          Set(Use(potion.slug) -> certainOutcome(List(PotionDrinked(source, potion))))

        case _ => sourceBeing.body match {
          case handedBody: Handed => handedBody.holding match {
            case Some(heldItem) =>
              Set(Use(item.slug) -> certainOutcome(List(Stashed(source, heldItem), Held(source, item))))

            case None =>
              Set(Use(item.slug) -> certainOutcome(List(Held(source, item))))
          }
          case _ =>
            Set()
        }
      }
      mappings
    }.toMap

    val directionActionTargets = DirectionalCommand.all.flatMap(dir =>
      actionTargetAtDirection(dir.direction).map((dir, _))
    ).toMap

    itemsActionTargets ++ directionActionTargets
  }

  def notify(notification: Notification) = this.modify(_.notificationHistory).using(notification :: _)
  def notify(notifications: List[Notification]) = this.modify(_.notificationHistory).using(notifications ++ _)

  def materialize(event: Event): GameState = {
    event match {

      case Held(source, item) =>
        val sourceBeing = dungeon.cells(source).being.get
        this
          .modify(_.dungeon.cells.at(source).being.each.itemBag)
          .using(_ - item)
          .modify(_.dungeon.cells.at(source).being.each.body.when[Handed].holding)
          .setTo(Some(item))
          .notify(ItemHeld(sourceBeing.descriptor, item))

      case Stashed(source, item) =>
        val sourceBeing = dungeon.cells(source).being.get
        this
          .modify(_.dungeon.cells.at(source).being.each.itemBag)
          .using(_ + item)
          .modify(_.dungeon.cells.at(source).being.each.body.when[Handed].holding)
          .setTo(None)
          .notify(ItemStash(sourceBeing.descriptor, item))

      case PickedUp(source, amount, item) =>
        val sourceBeing = dungeon.cells(source).being.get
        this
          .modify(_.dungeon.cells.at(source).being)
          .setTo(Some(sourceBeing.modify(_.itemBag).using(_ +(item, amount))))
          .modify(_.dungeon.cells.at(source).itemBag)
          .using(_ -(item, amount))
          .notify(TargetTaken(sourceBeing.descriptor, amount, item))

      case Dropped(source, amount, item) =>
        val sourceBeing = dungeon.cells(source).being.get
        this
          .modify(_.dungeon.cells.at(source).itemBag)
          .using(_ +(item, amount))
          .modify(_.dungeon.cells.at(source).being.each.itemBag)
          .using(_ -(item, amount))
          .notify(TargetDropsItem(sourceBeing.descriptor, amount, item))

      case Moved(source, direction) =>
        val sourceBeing = dungeon.cells(source).being.get
        val destinationPosition = source.towards(direction, 1)
        this
          .modify(_.dungeon.cells.at(source).being)
          .setTo(None)
          .modify(_.dungeon.cells.at(destinationPosition).being)
          .setTo(Some(sourceBeing))

      case PotionDrinked(source, potion) =>
        val sourceBeing = dungeon.cells(source).being.get
        this
          .modify(_.dungeon.cells.at(source).being.each)
          .setTo(potion.effect match {
            case FullyHeal => sourceBeing.modify(_.body.health).setTo(sourceBeing.body.fullHealth)
          })
          .modify(_.dungeon.cells.at(source).being.each.itemBag)
          .using(_ - potion)
          .notify(PotionDrank(sourceBeing.descriptor, potion))
          .notify(BeingAffected(sourceBeing.descriptor, potion.effect))

      case DoorOpened(source, target) =>
        val sourceBeing = dungeon.cells(source).being.get
        dungeon.cells(target).structure.get match {
          case openable: Openable => this
            .modify(_.dungeon.cells.at(target).structure)
            .setTo(Some(openable.opened))
            .notify(TargetOpened(sourceBeing.descriptor, openable))
          case _ => this
        }

      case Died(target) =>
        val targetBeing = dungeon.cells.get(target).get.being.get

        this
          .modify(_.dungeon.cells.at(target).being)
          .setTo(None)
          .notify(TargetDies(targetBeing.descriptor))

      case Damaged(source, target, damage) =>
        val sourceBeing = dungeon.cells(source).being.get
        val targetBeing = dungeon.cells.get(target).get.being.get
          .modify(_.body.health.value).using(_ - damage)

        this
          .modify(_.dungeon.cells.at(target).being)
          .setTo(Some(targetBeing))
          .notify(TargetHit(sourceBeing.descriptor, targetBeing.descriptor, damage))
    }
  }

}


sealed trait Command
trait DirectionalCommand extends Command {
  def direction: Direction
}

object DirectionalCommand {
  def all: Set[DirectionalCommand] = Set(Command.Up, Command.Down, Command.Right, Command.Left)
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
