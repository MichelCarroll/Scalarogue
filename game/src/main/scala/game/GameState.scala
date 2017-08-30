package game


import dungeon.{Cell, Dungeon}
import dungeon.generation.DungeonGenerator
import dungeon.generation.DungeonGenerator.GenerationError
import dungeon.generation.floorplan.{BSPTree, Floorplan, RandomBSPTreeParameters}
import game.being.{Handed, Player}
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
case class Stash(source: Position, item: Item) extends Event
case class Hold(source: Position, item: Item) extends Event
case class PickUp(source: Position, amount: Int, item: Item) extends Event
case class Drank(source: Position, potion: Potion) extends Event
case class DoorOpened(source: Position, target: Position) extends Event
case class Damaged(source: Position, target: Position, value: Int) extends Event


case class GameState(dungeon: Dungeon, revealedPositions: Set[Position], notificationHistory: List[Notification]) {

  type WeightedOutcome = (Int, List[Event])
  type ActionTarget = Set[WeightedOutcome]

  def actionTargetMapping(source: Position): Map[Command, ActionTarget] = {

    def certainOutcome(events: List[Event]) = Set(1 -> events)

    def actionTargetAtDirection(direction: Direction): Option[ActionTarget] = {
      val sourceBeing = dungeon.cells(source).being.get
      val target = source.towards(direction, 1)
      dungeon.cells.get(target) match {

        case Some(Cell(Some(_), _, _)) =>
          Some(sourceBeing.body.damageRange.values.toSet.map((damage:Int) =>
            1 -> List(Damaged(source, target, damage))
          ))

        case Some(Cell(None, Some(_:Openable), _)) =>
          Some(certainOutcome(List(DoorOpened(source, target))))

        case Some(cell@Cell(_, _, itemBag)) if cell.passable =>
          val pickUps = itemBag.items.map { case (item, n) => PickUp(target, n, item) }.toList
          Some(certainOutcome(Moved(source, direction) :: pickUps))

        case _ => None
      }
    }

    val sourceBeing = dungeon.cells(source).being.get

    val itemsActionTargets: Map[Command, ActionTarget] = sourceBeing.itemBag.items.keys.flatMap { item =>
      val mappings: Set[(Command, ActionTarget)] = item match {
        case potion: Potion =>
          Set(Use(potion.slug) -> certainOutcome(List(Drank(source, potion))))

        case _ => sourceBeing.body match {
          case handedBody: Handed => handedBody.holding match {
            case Some(heldItem) =>
              Set(Use(item.slug) -> certainOutcome(List(Stash(source, heldItem), Hold(source, item))))

            case None =>
              Set(Use(item.slug) -> certainOutcome(List(Hold(source, item))))
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


  def materialize(event: Event): GameState = {
    event match {

      case Hold(source, item) =>
        val sourceBeing = dungeon.cells(source).being.get
        this
          .modify(_.dungeon.cells.at(source).being.each.itemBag)
          .using(_ - item)
          .modify(_.dungeon.cells.at(source).being.each.body.when[Handed].holding)
          .setTo(Some(item))
          .modify(_.notificationHistory)
          .using(ItemHeld(sourceBeing.descriptor, item) :: _)

      case Stash(source, item) =>
        val sourceBeing = dungeon.cells(source).being.get
        this
          .modify(_.dungeon.cells.at(source).being.each.itemBag)
          .using(_ + item)
          .modify(_.dungeon.cells.at(source).being.each.body.when[Handed].holding)
          .setTo(None)
          .modify(_.notificationHistory)
          .using(ItemStash(sourceBeing.descriptor, item) :: _)

      case PickUp(source, amount, item) =>
        val sourceBeing = dungeon.cells(source).being.get
        this
          .modify(_.dungeon.cells.at(source).being)
          .setTo(Some(sourceBeing.modify(_.itemBag).using(_ +(item, amount))))
          .modify(_.dungeon.cells.at(source).itemBag)
          .using(_ -(item, amount))
          .modify(_.notificationHistory)
          .using(TargetTaken(sourceBeing.descriptor, amount, item) :: _)

      case Moved(source, direction) =>
        val sourceBeing = dungeon.cells(source).being.get
        val destinationPosition = source.towards(direction, 1)
        this
          .modify(_.dungeon.cells.at(source).being)
          .setTo(None)
          .modify(_.dungeon.cells.at(destinationPosition).being)
          .setTo(Some(sourceBeing))

      case Drank(source, potion) =>
        val sourceBeing = dungeon.cells(source).being.get
        val drinkNotification = PotionDrank(sourceBeing.descriptor, potion)
        val effectNotification = BeingAffected(sourceBeing.descriptor, potion.effect)
        this
          .modify(_.dungeon.cells.at(source).being.each)
          .setTo(potion.effect match {
            case FullyHeal => sourceBeing.modify(_.body.health).setTo(sourceBeing.body.fullHealth)
          })
          .modify(_.dungeon.cells.at(source).being.each.itemBag)
          .using(_ - potion)
          .modify(_.notificationHistory)
          .using(effectNotification :: drinkNotification :: _)

      case DoorOpened(source, target) =>
        val sourceBeing = dungeon.cells(source).being.get
        dungeon.cells(target).structure.get match {
          case openable: Openable => this
            .modify(_.dungeon.cells.at(target).structure).setTo(Some(openable.opened))
            .modify(_.notificationHistory).using(TargetOpened(sourceBeing.descriptor, openable) +: _)
          case _ => this
        }

      case Damaged(source, target, damage) =>
        val sourceBeing = dungeon.cells(source).being.get
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
