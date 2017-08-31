package ui

import dungeon._
import game.being._
import game._
import org.scalajs.dom
import math._
import org.scalajs.dom.ext.Color

object Colors {

  object Red {
    val Ketchup = Color("#F23C39")
    val Rust = Color("#A94335")
  }

  object Yellow {
    val Mustard = Color("#FBC736")
  }

}

object MainViewportDrawingContext {

  val viewportRange = (Player.lineOfLightRange * 2.0).ceil.toInt

  def viewport(cameraPosition: Position) = Area(
    Position(cameraPosition.x - viewportRange - 1, cameraPosition.y - viewportRange - 1),
    Position(cameraPosition.x + viewportRange + 1, cameraPosition.y + viewportRange + 1)
  )
}

/**
  * Created by MichelCarroll on 3/28/2017.
  */
class MainViewportDrawingContext(renderingContext: dom.CanvasRenderingContext2D) {

  val imageRepository = new ImageRepository(renderingContext)

  def ready = imageRepository.loaded

  val drawingArea = Area(
    Position(0, 0),
    Size(renderingContext.canvas.width, renderingContext.canvas.height)
  )

  val cellEdge = drawingArea.size.width / (MainViewportDrawingContext.viewportRange * 2 + 1).toDouble

  def drawFromPerspective(gameState: GameState, cameraPosition: Position) = {

    val viewport = MainViewportDrawingContext.viewport(cameraPosition)

    renderingContext.fillStyle = Color.Black.toString
    renderingContext.fillRect(
      drawingArea.position.x,
      drawingArea.position.y,
      drawingArea.size.width,
      drawingArea.size.height
    )

    val cellsInLineOfSight = gameState
      .revealedPositions
      .intersect(viewport.positions)
      .map(position => position -> gameState.dungeon.cells.get(position))
      .toMap

    cellsInLineOfSight.foreach {

      case (position, Some(Cell(Some(Being(descriptor, _, _, _)), structure, _))) => descriptor match {
        case Player => drawGridCharacter(position, '@', Colors.Red.Ketchup)
        case Spider => drawGridCharacter(position, 's', Colors.Red.Rust)
      }

      case (position, Some(Cell(None, Some(structure), _))) => structure match {
        case ClosedDoor => drawGridCharacter(position, '%', Colors.Yellow.Mustard)
        case OpenedDoor => drawGridCharacter(position, '.', Colors.Yellow.Mustard)
        case Downstairs => drawGridCharacter(position, '<', Colors.Yellow.Mustard)
        case Upstairs =>   drawGridCharacter(position, '>', Colors.Yellow.Mustard)
      }

      case (position, Some(Cell(None, None, itemBag))) if itemBag.items.nonEmpty =>
        itemBag.items.head._1 match {
          case Sword => drawGridCharacter(position, '|', Color.White)
          case Gold => drawGridCharacter(position, '$', Colors.Yellow.Mustard)
          case potion:Potion => drawGridCharacter(position, ',', Color.Red)
        }

      case (position, Some(Cell(_ ,_, _))) =>
        drawGridCharacter(position, '.', Colors.Yellow.Mustard)

      case (position, None) =>
        drawGridCharacter(position, '#', Colors.Yellow.Mustard)
    }

    //debug
    //    drawGrid()
    //    gameState.perimeterRays.foreach(ray => drawRay(ray, Color.Green))
    //    gameState.positionsWithinRangeTouchedByARay.foreach(drawDot(_, Color.Red))

    def centeredCanvasVector(vector: math.Vector) = {
      val topLeft = canvasVector(vector)
      CanvasPosition(topLeft.x + cellEdge / 2, topLeft.y + cellEdge / 2)
    }

    def centeredCanvasPosition(position: Position) = {
      val topLeft = canvasVector(position.vector)
      CanvasPosition(topLeft.x + cellEdge / 2, topLeft.y + cellEdge / 2)
    }

    def canvasVector(vector: math.Vector): CanvasPosition = CanvasPosition(
      (vector.x - viewport.topLeft.x) * cellEdge + drawingArea.position.x,
      (vector.y - viewport.topLeft.y) * cellEdge + drawingArea.position.y
    )

    def drawGridBackground(position: Position, color: Color): Unit = {
      val pos = canvasVector(position.vector)
      renderingContext.save()
      renderingContext.translate(pos.x, pos.y)
      renderingContext.fillStyle = color.toString()
      renderingContext.fillRect(0, 0, cellEdge, cellEdge)
      renderingContext.restore()
    }

    def drawGridCharacter(position: Position, character: Char, color: Color): Unit = {
      val pos = canvasVector(position.vector)
      renderingContext.save()
      renderingContext.translate(pos.x, pos.y)
      renderingContext.fillStyle = color.toString()
      renderingContext.font = "16px Verdana"
      renderingContext.textBaseline = "middle"
      renderingContext.textAlign = "center"
      renderingContext.fillText(character.toString, cellEdge / 2, cellEdge / 2, cellEdge)
      renderingContext.restore()
    }

    def drawGridImage(image: Image, position: Position): Unit = {
      val pos = canvasVector(position.vector)
      renderingContext.save()
      renderingContext.translate(pos.x, pos.y)
      renderingContext.drawImage(image.element, 0, 0, image.sourceSize.width, image.sourceSize.height, 0, 0, cellEdge, cellEdge)
      renderingContext.restore()
    }

    def drawRay(ray: Ray, color: Color): Unit = {
      val start = canvasVector(ray.origin)
      val end = canvasVector(ray.origin + ray.vector)
      renderingContext.lineWidth = 3
      renderingContext.strokeStyle = color.toString()
      renderingContext.beginPath()
      renderingContext.moveTo(start.x, start.y)
      renderingContext.lineTo(end.x, end.y)
      renderingContext.stroke()
    }

    def drawDot(position: Position, color: Color): Unit = {
      val pos = centeredCanvasPosition(position)
      renderingContext.beginPath()
      renderingContext.fillStyle = color.toString()
      renderingContext.arc(pos.x, pos.y, cellEdge / 6, 0, 2 * Math.PI)
      renderingContext.fill()
    }

    def drawGrid(): Unit = {

      def drawGridLine(from: CanvasPosition, to: CanvasPosition): Unit = {
        renderingContext.lineWidth = 1
        renderingContext.strokeStyle = Color.White.toString()
        renderingContext.beginPath()
        renderingContext.moveTo(from.x, from.y)
        renderingContext.lineTo(to.x, to.y)
        renderingContext.stroke()
      }

      for(x <- 0 to viewport.size.width)
        drawGridLine(CanvasPosition(x * cellEdge,0), CanvasPosition(x * cellEdge, viewport.size.height * cellEdge))
      for(y <- 0 to viewport.size.height)
        drawGridLine(CanvasPosition(0, y * cellEdge), CanvasPosition(viewport.size.width * cellEdge, y * cellEdge))
    }

    def drawPathToClosestSpider() = {
      gameState.dungeon.beingOfTypePositions(Spider) match {
        case spiderPositions if spiderPositions.isEmpty => None
        case spiderPositions =>
          gameState.dungeon.bestDirectionTo(
            cameraPosition,
            spiderPositions.minBy(_.manhattanDistanceTo(cameraPosition))
          ).map(cameraPosition.towards(_, 1))
      }
    } match {
      case Some(position) => drawDot(position, Color.Red)
      case _ =>
    }
  }


}
