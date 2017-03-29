import dungeon.generation.floorplan.{BSPTree, Floorplan, RandomBSPTreeParameters}
import ui.{DebugDrawingContext, GameDisplayAdapter}
import dungeon.generation.DungeonGenerator
import game.{Command, Game}
import math.Size
import org.scalajs.dom
import org.scalajs.dom.{CanvasRenderingContext2D, html}
import primitives.Ratio
import random.SimpleRNG

import scala.scalajs.js.annotation.JSExport


@JSExport
object Main {

  @JSExport
  def main(seed: Double, viewportCanvas: html.Canvas, minimapCanvas: html.Canvas, messagesList: html.UList, messageContainer: html.Div): Unit = {

    val game = new Game(
      seed = seed.toInt,
      displayAdapter = GameDisplayAdapter(viewportCanvas, minimapCanvas, messagesList, messageContainer)
    )

    dom.document.onkeydown = (e: dom.KeyboardEvent) => {
      Command.fromKeyCode(e.keyCode).foreach(game.executeTurn)
    }

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
