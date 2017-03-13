import math.{Direction, Position}

import scala.annotation.tailrec

trait Navigatable {

  val cells: Map[Position, Cell]

  def bestDirectionTo(source: Position, destination: Position): Option[Direction] = {

    def passableNeighborsOrGoal(a: Position, goal: Position): Set[(Direction, Position)] =
      a.neighbors
        .filter { results =>
          if (results._2 == goal)
            true
          else
            cells.get(results._2) match {
              case Some(cell@OpenCell(None,Some(ClosedDoor),_)) => true
              case Some(cell) if cell.passable => true
              case _ => false
            }
        }

    def heuristic(pos: Position) = pos manhattanDistanceTo destination

    case class OpenNodes(nodes: Map[Position, Node])
    case class ClosedNodes(nodes: Map[Position, Node])

    case class Node(g: Int, h: Int, parent: Option[(Position, Node)]) {
      def cost = g + h
    }

    @tailrec
    def iteration(open: OpenNodes, closed: ClosedNodes): Option[Direction] = {
      if(open.nodes.isEmpty)
        None
      else {
        val current = open.nodes.minBy(_._2.cost)

        if(current._1 == destination) {
          @tailrec
          def directionTo(pos: Position, node: Node): Option[Direction] = node.parent match {
            case Some(parent) => parent._2.parent match {
              case Some(_) => directionTo(parent._1, parent._2)
              case None => Some(source.neighbors.find(_._2 == pos).get._1)
            }
            case None => throw new RuntimeException("Should never happen")
          }
          directionTo(current._1, current._2)
        }
        else {
          val newOpen = passableNeighborsOrGoal(current._1, destination)
            .foldLeft(open) { (openNodes, neighbor) => {
              if(closed.nodes.contains(neighbor._2))
                openNodes
              else {
                OpenNodes(openNodes.nodes.updated(
                  neighbor._2,
                  Node(
                    current._2.g + 10,
                    heuristic(neighbor._2),
                    Some(current._1, current._2)
                  )
                ))
              }
            }}

          iteration(
            OpenNodes(newOpen.nodes - current._1),
            ClosedNodes(closed.nodes.updated(current._1, current._2)))
        }
      }
    }

    iteration(
      OpenNodes(Map(source -> Node(0, heuristic(source), None))),
      ClosedNodes(Map())
    )
  }
}