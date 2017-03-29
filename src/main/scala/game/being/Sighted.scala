package game.being

import dungeon.Dungeon
import math.{Angle, Position, Ray}

trait Sighted {

  val lineOfLightRange: Double

  def perimeterRays(position: Position) = {
    val nRays = 80
    val interval = Math.PI * 2 / nRays

    (0 until nRays)
      .map(_ * interval)
      .map(theta => math.Vector(lineOfLightRange, Angle(theta)))
      .map(vector => Ray(math.Vector(position.x + 0.5, position.y + 0.5), vector))
  }

  def positionsWithinRangeTouchedByPerimeterRay(position: Position, dungeon: Dungeon): Set[Position] = {
    var positionsTouched = Set[Position]()

    for(ray <- perimeterRays(position)) {
      val increment = 0.8
      val delta = ray.vector.unit * increment
      val numberIncrements = (ray.vector.magnitude / increment).toInt

      var touchedWall = false

      for(i <- 0 to numberIncrements) {
        val currentPosition = (ray.origin + delta * i).position

        if(!touchedWall) {
          positionsTouched = positionsTouched + currentPosition

          if(dungeon.cells.get(currentPosition).forall(_.opaque)) {
            touchedWall = true
          }
        }

      }
    }

    positionsTouched
  }
}