
import dungeon.generation.bsp.BSPTree
import math._
import org.scalajs.dom
import org.scalajs.dom.ext.Color


case class CanvasPosition(x: Double, y: Double)

class MainViewportDrawingContext(renderingContext: dom.CanvasRenderingContext2D) {

  val imageRepository = new ImageRepository(renderingContext)

  def ready = imageRepository.loaded

  val drawingArea = Area(
    Position(0, 0),
    Size(renderingContext.canvas.width, renderingContext.canvas.height)
  )
  val viewportRange = 5
  val cellEdge = drawingArea.size.width / (viewportRange * 2 + 1).toDouble

  def drawFromPerspective(gameState: GameState, cameraPosition: Position) = {

    val viewport = Player.viewport(cameraPosition)

    renderingContext.fillStyle = Color.Black.toString
    renderingContext.fillRect(
      drawingArea.position.x,
      drawingArea.position.y,
      drawingArea.size.width,
      drawingArea.size.height
    )

    val cellsInLineOfSight = Player
      .positionsWithinRangeTouchedByPerimeterRay(cameraPosition, gameState.dungeon)
      .intersect(viewport.positions)
      .map(position => position -> gameState.dungeon.cells.get(position))
      .toMap

    cellsInLineOfSight.foreach {
      case (position, Some(OpenCell(being, structure, items))) =>
        drawGridImage(imageRepository.floor, position)
        structure match {
          case Some(ClosedDoor) => drawGridImage(imageRepository.closed_door, position)
          case Some(OpenedDoor) => drawGridImage(imageRepository.open_door, position)
          case Some(Downstairs) => drawGridImage(imageRepository.downstairs, position)
          case Some(Upstairs) =>   drawGridImage(imageRepository.upstairs, position)
          case None =>
        }
        being match {
          case Some(Being(beingDescriptor, _, _)) => beingDescriptor match {
            case Player => drawGridImage(imageRepository.nugget, position)
            case Spider => drawGridImage(imageRepository.spider, position)
          }
          case None =>
        }
        items.foreach {
          case Gold(_) => drawGridImage(imageRepository.gold, position)
        }
      case (position, Some(ClosedCell)) =>
        drawGridImage(imageRepository.wall, position)
      case (position, None) =>
        drawGridImage(imageRepository.wall, position)
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
      gameState.dungeon.positionedBeings(Spider) match {
        case spiders if spiders.isEmpty => None
        case spiders =>
          gameState.dungeon.bestDirectionTo(
            cameraPosition,
            spiders.map(_.position).minBy(_.manhattanDistanceTo(cameraPosition))
          ).map(cameraPosition.towards(_, 1))
      }
    } match {
      case Some(position) => drawDot(position, Color.Red)
      case _ =>
    }
  }


}


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



class DebugDrawingContext(renderingContext: dom.CanvasRenderingContext2D, mapSize: Size) {


  def drawTree(tree: BSPTree, rng: RNG): RNG = {
    val positionedBSPLeaf = tree.positionedLeaves
    val (randomColors, newRNG) = RNG.sequence(List.fill(positionedBSPLeaf.size)(randomColor))(rng)
    positionedBSPLeaf
      .zip(randomColors)
      .foreach {
        case (BSPTree.PositionedLeaf(position, leafSize), color) => drawRect(position, leafSize, color)
      }
    newRNG
  }

  def drawTopologyCorridors(dungeonTopology: FloorplanGenerator.Topology) = {
    import FloorplanGenerator._
    dungeonTopology.corridors.foreach {
      case HorizontalCorridor(area) => drawArea(area, Color.Cyan)
      case VerticalCorridor(area)   => drawArea(area, Color.Yellow)
    }
  }

  def drawTopologyRooms(dungeonTopology: FloorplanGenerator.Topology) = {
    dungeonTopology.rooms.foreach(room => drawArea(room.area, Color.Black))
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
    val cellEdge = renderingContext.canvas.width / mapSize.width.toDouble
    renderingContext.fillStyle = color.toString()
    renderingContext.fillRect(position.x * cellEdge, position.y * cellEdge, size.width * cellEdge, size.height * cellEdge)
  }
}