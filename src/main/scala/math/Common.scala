package math


import math.Direction._


case class PrecisePosition(x: Double, y: Double)

case class Position(x: Int, y: Int) {

  def towards(direction: Direction, s: Int) = direction match {
    case Up => Position(x, y - s)
    case Down => Position(x, y + s)
    case Right => Position(x + s, y)
    case Left => Position(x - s, y)
  }

  def left(s: Int) = towards(Left, s)
  def up(s: Int) = towards(Up, s)
  def right(s: Int) = towards(Right, s)
  def down(s: Int) = towards(Down, s)

  def sides: Set[Position] = neighbors.map(_._2)

  def neighbors: Set[(Direction, Position)] = Direction.all.map(direction => (direction, towards(direction, 1)))

  def vector = Vector(x, y)

  def manhattanDistanceTo(other: Position): Int = Math.abs(other.x - x) + Math.abs(other.y - y)
}

case class Angle(radians: Double) {
  assert(radians >= 0)
  assert(radians < Math.PI * 2)
}

case class Vector(x: Double, y: Double) {
  def +(other: Vector) = Vector(x + other.x, y + other.y)
  def -(other: Vector) = Vector(x - other.x, y - other.y)
  def *(a: Double) = Vector(x * a, y * a)
  def magnitude = Math.sqrt(x * x + y * y)
  def unit = Vector(x / magnitude, y / magnitude)
  def ofMagnitude(newMagnitude: Int) = unit * newMagnitude
  def position = Position(x.toInt, y.toInt)
}

object Vector {
  val zero = Vector(0,0)
  def apply(magnitude: Double, angle: Angle): Vector = Vector(
    x = magnitude * Math.cos(angle.radians),
    y = magnitude * Math.sin(angle.radians)
  )
}

case class Ray(origin: Vector, vector: Vector)

sealed trait Shape
case object SkewedVertically extends Shape
case object SkewedHorizontally extends Shape
case object Normal extends Shape

case class RatioSize(width: Double, height: Double) {

  def surfaceArea = width * height

  def shape(skewdnessCutoff: Double): Shape = width / (width + height) match {
    case s if s > skewdnessCutoff => SkewedHorizontally
    case s if s < (1 - skewdnessCutoff) => SkewedVertically
    case _ => Normal
  }
}

case class Area(position: Position, size: Size) {

  def towards(direction: Direction, s: Int) = Area(position.towards(direction, s), size)


  def center =
    Position(
      position.x + (size.width / 2.0).floor.toInt,
      position.y + (size.height / 2.0).floor.toInt
    )


  def minX = position.x
  def minY = position.y
  def maxX = position.x + size.width - 1
  def maxY = position.y + size.height - 1

  def positions: Set[Position] =
    (for(
      x <- minX to maxX;
      y <- minY to maxY
    ) yield Position(x, y)).toSet

  def contains(position: Position) = positions.contains(position)

  def topLeft: Position = position
  def topRight: Position = position.right(size.width - 1)
  def bottomLeft: Position = position.down(size.height - 1)
  def bottomRight: Position = position.down(size.height - 1).right(size.width - 1)

  def edge(direction: Direction): Area = direction match {
    case Up    => Area(topLeft, topRight)
    case Down  => Area(bottomLeft, bottomRight)
    case Left  => Area(topLeft, bottomLeft)
    case Right => Area(topRight, bottomRight)
  }

  def adjacencyLine(direction: Direction): Area = edge(direction).towards(direction, 1)

  def shrink(s: Int) = Area(Position(s,s), Size(size.width - s * 2, size.height - s * 2))
}

sealed trait Direction
object Direction {
  case object Up extends Direction
  case object Down extends Direction
  case object Right extends Direction
  case object Left extends Direction

  def all: Set[Direction] = Set(Up, Down, Right, Left)
}

object Area {

  def apply(topLeft: Position, bottomRight: Position): Area =
    Area(
      topLeft,
      Size(
        bottomRight.x - topLeft.x + 1,
        bottomRight.y - topLeft.y + 1
      )
    )

}

case class Size(width: Int, height: Int) {

  def partitionHorizontally(ratio: Double) = (
    Size((width * ratio).round.toInt, height),
    Size((width * (1 - ratio)).round.toInt, height)
    )

  def partitionVertically(ratio: Double) = (
    Size(width, (height * ratio).round.toInt),
    Size(width, (height * (1 - ratio)).round.toInt)
    )

  def empty = width <= 0 || height <= 0
}