
import dungeon.generation.bsp.BSPTree
import math.{SimpleRNG, Size}
import org.scalajs.dom
import org.scalajs.dom.{CanvasRenderingContext2D, html}

import scala.scalajs.js.annotation.JSExport


@JSExport
object Main {

  @JSExport
  def main(viewportCanvas: html.Canvas, minimapCanvas: html.Canvas, messagesList: html.UList, messageContainer: html.Div): Unit = {

    val game = new Game(GameDisplayAdapter(viewportCanvas, minimapCanvas, messagesList, messageContainer))

    dom.document.onkeydown = (e: dom.KeyboardEvent) => {
      Command.fromKeyCode(e.keyCode).foreach(game.executeTurn)
    }

  }

  @JSExport
  def dungeonGeneration(viewportCanvas: html.Canvas): Unit = {

    val rng = SimpleRNG(192394)
    val gridSize = Size(25, 25)
    val (randomTree, newRng) = BSPTree.buildRandomTree(minLeafSurface = 0.02, maxLeafSurface = 0.15, skewdnessCutoff = 0.8)(rng)
    val ((floorplanTopology, floorplan), newRng2) = FloorplanGenerator.generate(randomTree, gridSize)(newRng)
    val (dungeonEither, newRng3) = DungeonGenerator.generate(floorplan)(newRng2)
    (dungeonEither, newRng3)

    val debugDrawingContext = new DebugDrawingContext(
      renderingContext = viewportCanvas.getContext("2d").asInstanceOf[CanvasRenderingContext2D],
      mapSize = gridSize
    )

    debugDrawingContext.drawTree(randomTree, newRng3)
//    debugDrawingContext.drawTopologyRooms(floorplanTopology)
  }
}