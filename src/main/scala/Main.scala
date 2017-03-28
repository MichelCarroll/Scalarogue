
import dungeon.generation.bsp.BSPTree
import math.{SimpleRNG, Size}
import org.scalajs.dom
import org.scalajs.dom.{CanvasRenderingContext2D, html}

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
      val (randomTree, newRng) = BSPTree.buildRandomTree(size = gridSize)(rng)
      val ((floorplanTopology, floorplan), newRng2) = FloorplanGenerator.generate(randomTree, gridSize)(newRng)
      val (dungeonEither, newRng3) = DungeonGenerator.generate(floorplan)(newRng2)
      (dungeonEither, newRng3)

      val debugDrawingContext = new DebugDrawingContext(
        renderingContext = dungeonViewport.getContext("2d").asInstanceOf[CanvasRenderingContext2D],
        mapSize = gridSize
      )

      debugDrawingContext.drawTree(randomTree, newRng3)
      debugDrawingContext.drawTopologyRooms(floorplanTopology)
      debugDrawingContext.drawFloorplan(floorplan)
    }

    (1 to 10).foreach(_ => createNewDungeon())

  }
}