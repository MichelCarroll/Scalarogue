package ui

import dungeon.generation._
import dungeon.generation.floorplan.{BSPTree, Floorplan}
import dungeon.generation.floorplan.Floorplan._
import math._
import random.RNG
import org.scalajs.dom
import org.scalajs.dom.ext.Color

/**
  * Created by MichelCarroll on 3/28/2017.
  */
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

  def drawTopologyCorridors(dungeonTopology: Floorplan.Topology) = {
    dungeonTopology.corridors.foreach {
      case HorizontalCorridor(area) => drawArea(area, Color.Cyan)
      case VerticalCorridor(area)   => drawArea(area, Color.Yellow)
    }
  }

  def drawTopologyRooms(dungeonTopology: Floorplan.Topology) = {
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
