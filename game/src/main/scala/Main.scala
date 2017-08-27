import dungeon.generation.floorplan.{BSPTree, Floorplan, RandomBSPTreeParameters}
import ui.{DebugDrawingContext, GameDisplayAdapter}
import math.Size
import dungeon.generation.DungeonGenerator
import game.Command.UseItem
import game.{Command, Game, ItemSlug}

import scala.scalajs.js
import org.scalajs.dom
import org.scalajs.dom.{CanvasRenderingContext2D, html}
import primitives.Ratio
import random.SimpleRNG

import scala.scalajs.js.annotation.{JSExport, JSExportTopLevel}

@JSExportTopLevel("GameHooks")
class GameHooks(game: Game) {

  def useItem(itemSlugValue: String): Unit = {
    game.executeTurn(UseItem(ItemSlug(itemSlugValue)))
  }

}

@JSExportTopLevel("Main")
object Main {

  @JSExport
  def start(options: js.Dictionary[Any]): GameHooks = {

    val seed = options("seed").toString.toLong
    val viewportCanvas = options("viewport").asInstanceOf[html.Canvas]
    val minimapCanvas = options("minimap").asInstanceOf[html.Canvas]
    val onUpdateState = options("onUpdateState").asInstanceOf[js.Function1[Any, Any]]

    val game = new Game(
      seed = seed,
      displayAdapter = GameDisplayAdapter(viewportCanvas, minimapCanvas, onUpdateState)
    )

    dom.document.onkeydown = (e: dom.KeyboardEvent) => {
      Command.fromKeyCode(e.keyCode).foreach(game.executeTurn)
    }

    new GameHooks(game)
  }

  @JSExport
  def dungeonGeneration(seedGenerator: scalajs.js.Function0[Double], dungeonViewportGenerator: scalajs.js.Function0[html.Canvas]): Unit = {

    def createNewDungeon(): Unit = {
      val seed = seedGenerator()
      val dungeonViewport = dungeonViewportGenerator()

      val rng = SimpleRNG(seed.toInt)
      val gridSize = Size(50, 50)

      val debugDrawingContext = new DebugDrawingContext(
        renderingContext = dungeonViewport.getContext("2d").asInstanceOf[CanvasRenderingContext2D],
        mapSize = gridSize
      )

      val generator = for {
        bspTree <- BSPTree.generate(
          RandomBSPTreeParameters(
            size = gridSize,
            minLeafEdgeLength = 3,
            minLeafSurfaceRelativeToTotal = Ratio(0.08)
          ))
        floorplan <- Floorplan.generate(bspTree, gridSize)
        dungeonEither <- DungeonGenerator.generate(floorplan)
      } yield (bspTree, floorplan, dungeonEither)

      generator(rng)._1 match {
        case (bspTree, floorplan, dungeonEither) =>
          debugDrawingContext.drawTree(bspTree)(rng)
          debugDrawingContext.drawTopologyCorridors(floorplan.topology)
          debugDrawingContext.drawFloorplan(floorplan)
      }

    }

    (1 to 10).foreach(_ => createNewDungeon())

  }
}
