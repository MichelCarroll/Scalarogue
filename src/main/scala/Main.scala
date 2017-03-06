
import org.scalajs.dom
import org.scalajs.dom.html

import scala.scalajs.js.annotation.JSExport
import scala.concurrent.ExecutionContext.Implicits.global


@JSExport
object Main {

  @JSExport
  def main(canvas: html.Canvas): Unit = {

    val canvasSize = Size(2500, 2000)
    val mainGameViewportArea = Area(Position(0, 0), Size(2000, 2000))
    val minimapViewportArea =  Area(Position(2000, 0), Size(500, 500))

    val renderingContext = canvas.getContext("2d").asInstanceOf[dom.CanvasRenderingContext2D]
    renderingContext.canvas.width = canvasSize.width
    renderingContext.canvas.height = canvasSize.height

    val mainViewportDrawingContext = new MainViewportDrawingContext(
      renderingContext = renderingContext,
      drawingArea = mainGameViewportArea
    )
    val minimapDrawingContext = new MinimapViewportDrawingContext(
      renderingContext = renderingContext,
      drawingArea = minimapViewportArea
    )

    var gameState = GameState.start(1512512)

    def redraw(): Unit = {
      mainViewportDrawingContext.draw(gameState)
      minimapDrawingContext.draw(gameState)
    }

    dom.document.onkeydown = (e: dom.KeyboardEvent) => {
      gameState = PlayerCommand.fromKeyCode(e.keyCode)
        .map(gameState.applyPlayerCommand(_))
        .getOrElse(gameState)
      redraw()
    }

    mainViewportDrawingContext.ready.map(_ => redraw())
  }
}