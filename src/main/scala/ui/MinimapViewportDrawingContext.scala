package ui

import game.GameState
import game.being.Player
import org.scalajs.dom
import math._
import org.scalajs.dom.ext.Color
import dungeon.OpenCell

/**
  * Created by MichelCarroll on 3/28/2017.
  */
class MinimapViewportDrawingContext(renderingContext: dom.CanvasRenderingContext2D) {

  val drawingArea = Area(
    Position(0, 0),
    Size(renderingContext.canvas.width, renderingContext.canvas.height)
  )

  def draw(gameState: GameState, cameraPosition: Position) = {

    val viewport = Player.viewport(cameraPosition)

    renderingContext.fillStyle = Color.Black.toString
    renderingContext.fillRect(
      drawingArea.position.x,
      drawingArea.position.y,
      drawingArea.size.width,
      drawingArea.size.height
    )

    val cellEdge = drawingArea.size.width / gameState.dungeon.area.size.width

    gameState.dungeon.cells.foreach {
      case (position, OpenCell(_,_,_)) => drawCell(position, Color.Green)
      case _ =>
    }

    def drawCell(position: Position, color: Color) = {
      renderingContext.fillStyle = color.toString()
      renderingContext.fillRect(
        drawingArea.position.x + position.x * cellEdge,
        drawingArea.position.y + position.y * cellEdge,
        cellEdge,
        cellEdge
      )
    }

    renderingContext.strokeStyle = Color.White.toString
    renderingContext.lineWidth = 2
    renderingContext.strokeRect(
      drawingArea.position.x + viewport.position.x * cellEdge,
      drawingArea.position.y + viewport.position.y * cellEdge,
      viewport.size.width * cellEdge,
      viewport.size.height * cellEdge
    )

  }


}
