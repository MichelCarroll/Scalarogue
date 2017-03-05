
import org.scalajs.dom
import org.scalajs.dom.ext.Color
import org.scalajs.dom.html

import scala.scalajs.js.annotation.JSExport

@JSExport
object Main {

  @JSExport
  def main(canvas: html.Canvas): Unit = {

    val rng = SimpleRNG(212382)
    val gridSize = Size(100, 100)
    val cellEdge = 5
    val (randomTree, newRng) = BSPTree.buildRandomTree(minLeafSurface = 0.02, maxLeafSurface = 0.15, skewdnessCutoff = 0.8)(rng)
    val ((topology, tiles), newRng2) = DungeonTileGenerator.generate(randomTree, gridSize)(newRng)

    val drawingContext = canvas.getContext("2d").asInstanceOf[dom.CanvasRenderingContext2D]
    drawingContext.canvas.width = gridSize.width * cellEdge
    drawingContext.canvas.height = gridSize.height * cellEdge

    def drawCell(position: Position, color: Color) = drawRect(position, Size(1, 1), color)
    def drawArea(area: Area, color: Color) = drawRect(area.position, area.size, color)

    def drawRect(position: Position, size: Size, color: Color) = {
      drawingContext.fillStyle = color.toString()
      drawingContext.fillRect(position.x * cellEdge, position.y * cellEdge, size.width * cellEdge, size.height * cellEdge)
    }

    def randomColor: RNG.Rand[Color] = rng => {
      val (r, random2) = RNG.nextPositiveInt(255)(rng)
      val (g, random3) = RNG.nextPositiveInt(255)(random2)
      val (b, random4) = RNG.nextPositiveInt(255)(random3)
      (Color(r,g,b), random4)
    }

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

    def drawDungeonTopology(dungeonTopology: DungeonTopology) = {
      dungeonTopology.rooms.foreach(room => drawArea(room.area, Color.Black))

      dungeonTopology.corridors.foreach {
        case HorizontalCorridor(area) => drawArea(area, Color.Cyan)
        case VerticalCorridor(area)   => drawArea(area, Color.Yellow)
      }
    }

    def drawTiles(floorTiles: Map[Position, Tile]) = {
      floorTiles.foreach {
        case (position, RoomTile) => drawCell(position, Color.Green)
        case (position, DoorTile) => drawCell(position, Color.Black)
        case (position, CorridorTile) => drawCell(position, Color.Red)
      }
    }

//    drawTree(randomTree, gridSize, newRng)
    drawDungeonTopology(topology)
//    drawTiles(tiles)
  }
}