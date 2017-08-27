package game


import dungeon.{Dungeon, Cell}
import dungeon.generation.DungeonGenerator
import dungeon.generation.DungeonGenerator.GenerationError
import dungeon.generation.floorplan.{BSPTree, Floorplan, RandomBSPTreeParameters}
import game.being.{Being, Player}
import math.{Position, Size}
import primitives.Ratio
import random.RNG
import random.RNG._
import com.softwaremill.quicklens._


case class GameState(dungeon: Dungeon, rng: RNG, revealedPositions: Set[Position], notificationHistory: List[Notification]) {
  import Command._

  def applyCommand(sourcePosition: Position, command: Command): GameState = {

    def attemptNewPosition(sourceBeing: Being, destinationPosition: Position): GameState =

      dungeon.cells.get(destinationPosition) match {

        case Some(cell@Cell(Some(being: Being), structure, itemsOnGround)) =>

          val ((newBeing, notificationOpt), newRng) = sourceBeing.descriptor
            .damageRange
            .randomDamage
            .flatMap(being.hit)(rng)

          def hitNotification = TargetHit(sourceBeing.descriptor, newBeing.descriptor, notificationOpt)
          def deathNotifications =
            if(newBeing.body.dead)
              TargetDies(newBeing.descriptor) :: newBeing.itemBag.items.map {
                case (item, amount) => TargetDropsItem(being.descriptor, amount, item)
              }.toList
            else List()

          (
            if (newBeing.body.dead)
              this
                .modify(_.dungeon.cells.at(destinationPosition).being).setTo(None)
                .modify(_.dungeon.cells.at(destinationPosition).itemBag).using(_ + newBeing.itemBag)
            else
              this
                .modify(_.dungeon.cells.at(destinationPosition).being).setTo(Some(newBeing))
            )
            .modify(_.rng).setTo(newRng)
            .modify(_.notificationHistory).using(hitNotification :: deathNotifications ++: _)


        case Some(cell@Cell(None, Some(openable: Openable), items)) =>
          this
            .modify(_.dungeon.cells.at(destinationPosition)).using {
              case cell@Cell(_,_,_) => cell.modify(_.structure).setTo(Some(openable.opened))
            }
            .modify(_.notificationHistory).using(TargetOpened(sourceBeing.descriptor, openable) +: _)

        case Some(cell@Cell(None, structure, itemBag)) if cell.passable =>
          this
            .modify(_.dungeon.cells.at(sourcePosition).being)
            .setTo(None)
            .modify(_.dungeon.cells.at(destinationPosition).being)
            .setTo(Some(sourceBeing.modify(_.itemBag).using(_ + itemBag)))
            .modify(_.dungeon.cells.at(destinationPosition).itemBag)
            .setTo(ItemBag.empty)
            .modify(_.notificationHistory)
            .using(itemBag.items.map {
              case (item, amount) => TargetTaken(sourceBeing.descriptor, amount, item)
            }.toList ++: _)

        case _ => this

      }

    dungeon.cells.get(sourcePosition) match {
      case Some(Cell(Some(being@Being(_,_,_,_)), _, _)) => command match {
          case Up => attemptNewPosition(being, sourcePosition.up(1))
          case Down => attemptNewPosition(being, sourcePosition.down(1))
          case Left => attemptNewPosition(being, sourcePosition.left(1))
          case Right => attemptNewPosition(being, sourcePosition.right(1))
          case UseItem(itemSlug) =>
            being.itemBag.get(itemSlug) match {
              case Some(item) =>
                this
                  .modify(_.dungeon.cells.at(sourcePosition).being.each.itemBag)
                  .using(_ - item)
                  .modify(_.dungeon.cells.at(sourcePosition).being.each)
                  .using(_.use(item))

              case None => this
            }
        }
      case _ => throw new Exception("No being in this tile")
    }
  }

}


sealed trait Command
object Command {
  case object Up extends Command
  case object Down extends Command
  case object Right extends Command
  case object Left extends Command
  case class UseItem(itemSlug: ItemSlug) extends Command

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
      case (Right(dungeon), newRng) => dungeon.playerPosition match {

        case Some(position) =>(
          GameState(
            dungeon = dungeon,
            rng = newRng,
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
