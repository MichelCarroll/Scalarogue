package ui

import dungeon.generation._
import dungeon.generation.floorplan.{BSPTree, Floorplan}
import dungeon.generation.floorplan.Floorplan._
import math._
import random.RNG._
import org.scalajs.dom
import org.scalajs.dom.ext.Color

/**
  * Created by MichelCarroll on 3/28/2017.
  */
class DebugDrawingContext(renderingContext: dom.CanvasRenderingContext2D, mapSize: Size) {


  def drawTree(tree: BSPTree): Rand[Unit] = {
    val positionedLeaves = tree.positionedLeaves
    sequence(List.fill(positionedLeaves.size)(randomColor))
      .map(randomColors =>
        positionedLeaves
          .zip(randomColors)
          .foreach {
            case (BSPTree.PositionedLeaf(position, leafSize), color) => drawRect(position, leafSize, color)
          }
      )
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

  private def randomColor: Rand[Color] = for {
    r <- nextPositiveInt(255)
    g <- nextPositiveInt(255)
    b <- nextPositiveInt(255)
  } yield Color(r,g,b)

  private def drawCell(position: Position, color: Color) = drawRect(position, Size(1, 1), color)
  private def drawArea(area: Area, color: Color) = drawRect(area.position, area.size, color)

  private def drawRect(position: Position, size: Size, color: Color) = {
    val cellEdge = renderingContext.canvas.width / mapSize.width.toDouble
    renderingContext.fillStyle = color.toString()
    renderingContext.fillRect(position.x * cellEdge, position.y * cellEdge, size.width * cellEdge, size.height * cellEdge)
  }
}
