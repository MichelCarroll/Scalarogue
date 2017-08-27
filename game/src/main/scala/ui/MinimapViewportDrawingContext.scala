package ui

import game.GameState
import game.being.Player
import org.scalajs.dom
import math._
import org.scalajs.dom.ext.Color
import dungeon.Cell

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

    val openPositions = gameState.dungeon.cells.flatMap {
      case (position, Cell(_,_,_)) => Some(position)
      case _ => None
    }.toSet

    def drawCell(position: Position, color: Color) = {
      renderingContext.fillStyle = color.toString()
      renderingContext.fillRect(
        drawingArea.position.x + position.x * cellEdge,
        drawingArea.position.y + position.y * cellEdge,
        cellEdge,
        cellEdge
      )
    }

    (viewport.positions intersect openPositions intersect gameState.revealedPositions)
      .foreach(position => drawCell(position, Color.Green))

    (openPositions intersect gameState.revealedPositions diff viewport.positions)
      .foreach(position => drawCell(position, Color(50,50,50)))


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
