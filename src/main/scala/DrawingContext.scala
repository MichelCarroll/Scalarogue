
import org.scalajs.dom
import org.scalajs.dom.ext.Color


case class CanvasPosition(x: Double, y: Double)

class MainViewportDrawingContext(renderingContext: dom.CanvasRenderingContext2D, drawingArea: Area) {

  import Structure._

  val imageRepository = new ImageRepository(renderingContext)

  def ready = imageRepository.loaded

  val viewportRange = 5
  val cellEdge = drawingArea.size.width / (viewportRange * 2 + 1).toDouble

  def draw(gameState: GameState) = {

    val cellsInViewport = gameState.player.viewport
      .positions
      .map(position => position -> gameState.dungeon.cells.get(position))
      .toMap

    cellsInViewport.foreach {
      case (position, Some(OpenCell(being, structure, _))) =>
        drawGridImage(imageRepository.floor, position)
        structure match {
          case Some(ClosedDoor) => drawGridImage(imageRepository.closed_door, position)
          case Some(OpenedDoor) => drawGridImage(imageRepository.open_door, position)
          case Some(Downstairs) => drawGridImage(imageRepository.downstairs, position)
          case Some(Upstairs) =>   drawGridImage(imageRepository.upstairs, position)
          case None =>
        }
      case (position, Some(ClosedCell)) =>
        drawGridImage(imageRepository.darkness, position)
      case (position, None) =>
        drawGridImage(imageRepository.darkness, position)
    }

    drawGridImage(imageRepository.nugget, gameState.player.position)

    def drawGridImage(image: Image, position: Position): Unit = {
      val canvasPosition = CanvasPosition(
        (position.x - gameState.player.viewport.topLeft.x) * cellEdge,
        (position.y - gameState.player.viewport.topLeft.y) * cellEdge
      )
      renderingContext.save()
      renderingContext.translate(canvasPosition.x + drawingArea.position.x, canvasPosition.y + drawingArea.position.y)
      renderingContext.drawImage(image.element, 0, 0, image.sourceSize.width, image.sourceSize.height, 0, 0, cellEdge, cellEdge)
      renderingContext.restore()
    }

  }


}


class MinimapViewportDrawingContext(renderingContext: dom.CanvasRenderingContext2D, drawingArea: Area) {


  def draw(gameState: GameState) = {

    renderingContext.fillStyle = Color.Black.toString
    renderingContext.fillRect(
      drawingArea.position.x,
      drawingArea.position.y,
      drawingArea.size.width,
      drawingArea.size.height
    )

    val cellEdge = drawingArea.size.width / gameState.dungeon.size.width

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
    renderingContext.lineWidth = 5
    renderingContext.strokeRect(
      drawingArea.position.x + gameState.player.viewport.position.x * cellEdge,
      drawingArea.position.y + gameState.player.viewport.position.y * cellEdge,
      gameState.player.viewport.size.width * cellEdge,
      gameState.player.viewport.size.height * cellEdge
    )

  }


}



class DebugDrawingContext(renderingContext: dom.CanvasRenderingContext2D, size: Size) {

  val cellEdge = 5
  renderingContext.canvas.width = size.width * cellEdge
  renderingContext.canvas.height = size.height * cellEdge

  def drawTree(tree: BSPTree, size: Size, rng: RNG): RNG = {
    val positionedBSPLeaf = tree.positionedLeaves(size)
    val (randomColors, newRNG) = RNG.sequence(List.fill(positionedBSPLeaf.size)(randomColor))(rng)
    positionedBSPLeaf
      .zip(randomColors)
      .foreach {
        case (BSPTree.PositionedLeaf(position, leafSize), color) => drawRect(position, leafSize, color)
      }
    newRNG
  }

  def drawFloorplanTopology(dungeonTopology: FloorplanGenerator.Topology) = {
    dungeonTopology.rooms.foreach(room => drawArea(room.area, Color.Black))

    import FloorplanGenerator._
    dungeonTopology.corridors.foreach {
      case HorizontalCorridor(area) => drawArea(area, Color.Cyan)
      case VerticalCorridor(area)   => drawArea(area, Color.Yellow)
    }
  }

  def drawFloorplan(floorplan: Floorplan) = {
    floorplan.positionedTiles.foreach {
      case (position, RoomTile) => drawCell(position, Color.Green)
      case (position, DoorTile) => drawCell(position, Color.Black)
      case (position, CorridorTile) => drawCell(position, Color.Red)
    }
  }

  private def randomColor: RNG.Rand[Color] = rng => {
    val (r, random2) = RNG.nextPositiveInt(255)(rng)
    val (g, random3) = RNG.nextPositiveInt(255)(random2)
    val (b, random4) = RNG.nextPositiveInt(255)(random3)
    (Color(r,g,b), random4)
  }


  private def drawCell(position: Position, color: Color) = drawRect(position, Size(1, 1), color)
  private def drawArea(area: Area, color: Color) = drawRect(area.position, area.size, color)

  private def drawRect(position: Position, size: Size, color: Color) = {
    renderingContext.fillStyle = color.toString()
    renderingContext.fillRect(position.x * cellEdge, position.y * cellEdge, size.width * cellEdge, size.height * cellEdge)
  }
}


/*

  val innerCharacterWidth = 15
  val alphabetIndexMapping = (
    ('a' to 'z').zip(0 to 25)
      ++ ('0' to '9').zip(26 to 35)
    ).toMap

  private def drawText(canvasPosition: CanvasPosition, text: String): Unit = {
    val imageHeight = 20
    val imageWidth = 20

    renderingContext.save()
    renderingContext.translate(canvasPosition.x, canvasPosition.y)

    text
      .toLowerCase
      .map(alphabetIndexMapping.get)
      .foreach {
        case Some(index) =>
          renderingContext.translate(innerCharacterWidth, 0)
          renderingContext.drawImage(imageRepository.alphabet.element, index * 50, 0, 50, 50, -imageWidth / 2, -imageHeight / 2, imageWidth, imageHeight)

        case _ =>
          renderingContext.translate(innerCharacterWidth, 0)
      }

    renderingContext.restore()
  }
 */