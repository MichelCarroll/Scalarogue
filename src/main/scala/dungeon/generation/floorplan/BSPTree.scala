

package dungeon.generation.floorplan

import math._
import primitives.NonNeg._
import primitives.Ratio
import random.{RNG, SimpleRNG}
import random.RNG._

sealed trait BSPTree {
  import BSPTree._

  def size: Size

  def positionedLeaves: Set[PositionedLeaf] = {

    def inner(tree: BSPTree, position: Position): Set[PositionedLeaf] = tree match {
      case VerticalBranch(top, bottom) =>
        inner(top, position) ++
        inner(bottom, position.down(top.size.height))

      case HorizontalBranch(left, right) =>
        inner(left, position) ++
        inner(right, position.right(left.size.width))

      case Leaf(size) => Set(PositionedLeaf(position, size))
    }

    inner(this, Position(0,0))
  }

}

case class RandomBSPTreeParameters(size: Size, minLeafEdgeLength: NonNegInt, minLeafSurfaceRelativeToTotal: Ratio) {
  def minLeafSurface = size.surface * minLeafSurfaceRelativeToTotal.value
}

object BSPTree {
  case class VerticalBranch(top: BSPTree, bottom: BSPTree) extends BSPTree {
    def size = Size(top.size.width, top.size.height + bottom.size.height)
  }
  case class HorizontalBranch(left: BSPTree, right: BSPTree) extends BSPTree {
    def size = Size(left.size.width + right.size.width, left.size.height)
  }
  case class Leaf(size: Size) extends BSPTree

  case class PositionedLeaf(position: Position, size: Size)

  def generate(parameters: RandomBSPTreeParameters): Rand[BSPTree] = {

    def inner(size: Size): Rand[BSPTree] = {

      def verticalBranch: Rand[BSPTree] =
        RNG.nextPositiveInt(size.height - parameters.minLeafEdgeLength.value * 2)
          .map(_ + parameters.minLeafEdgeLength.value)
          .flatMap(topHeight =>
              inner(Size(size.width, topHeight))
              .combine(
                inner(Size(size.width, size.height - topHeight))
              )
          )
          .map { case (top, bottom) => VerticalBranch(top, bottom) }

      def horizontalBranch: Rand[BSPTree] =
        RNG.nextPositiveInt(size.width - parameters.minLeafEdgeLength.value * 2)
          .map(_ + parameters.minLeafEdgeLength.value)
          .flatMap(leftWidth =>
            inner(Size(leftWidth, size.height))
              .combine(
                inner(Size(size.width  - leftWidth, size.height))
              )
          )
          .map { case (left, right) => HorizontalBranch(left, right) }

      def randomOrientationBranch: Rand[BSPTree] =
        RNG.nextBoolean.flatMap {
          case true =>  verticalBranch
          case false => horizontalBranch
        }

      if(size.surface > parameters.minLeafSurface)
        size.shape match {
          case Square if size.width > parameters.minLeafEdgeLength.value * 2 => randomOrientationBranch
          case SkewedHorizontally if size.width > parameters.minLeafEdgeLength.value * 2  => horizontalBranch
          case SkewedVertically if size.height > parameters.minLeafEdgeLength.value * 2 => verticalBranch
        }
      else unit(Leaf(size))
    }

    inner(parameters.size)
  }

  trait Tree
  case class Branch(left: Tree, right: Tree) extends Tree
  case object Leaf extends Tree

  def generate: Rand[Tree] = rng => {

    def inner: Rand[Tree] = rng => {
      val (shouldBranchOut, newRng) = RNG.nextBoolean(rng)

      if(shouldBranchOut) {
        val (left, newRng1) = inner(newRng)
        val (right, newRng2) = inner(newRng1)
        (Branch(left, right), newRng2)
      }
      else
        (Leaf, newRng)
    }

    inner(rng)
  }

  println(generate(SimpleRNG(272)))

}


//Branch(Leaf,Branch(Branch(Leaf,Leaf),Leaf))
