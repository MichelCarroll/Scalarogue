

package dungeon.generation.bsp

import math._

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



object BSPTree {
  case class VerticalBranch(top: BSPTree, bottom: BSPTree) extends BSPTree {
    def size = Size(top.size.width, top.size.height + bottom.size.height)
  }
  case class HorizontalBranch(left: BSPTree, right: BSPTree) extends BSPTree {
    def size = Size(left.size.width + right.size.width, left.size.height)
  }
  case class Leaf(size: Size) extends BSPTree

  case class PositionedLeaf(position: Position, size: Size)

  def buildRandomTree(size: Size, minEdgeLength: Int = 5, minSurface: Int = 150)(rng: RNG): (BSPTree, RNG) = {
    var varRng: RNG = rng

    def inner(size: Size, verticalSplit: Boolean): BSPTree = {

      def verticalBranch = {
        val leeway = size.height - minEdgeLength * 2
        val (topHeightOffset, newRng) = RNG.nextPositiveInt(leeway)(varRng)
        varRng = newRng
        val topHeight = minEdgeLength + topHeightOffset
        VerticalBranch(
          inner(Size(size.width, topHeight), verticalSplit = false),
          inner(Size(size.width, size.height - topHeight), verticalSplit = false)
        )
      }

      def horizontalBranch = {
        val leeway = size.width - minEdgeLength * 2
        val (topWidthOffset, newRng) = RNG.nextPositiveInt(leeway)(varRng)
        varRng = newRng
        val leftWidth = minEdgeLength + topWidthOffset
        HorizontalBranch(
          inner(Size(leftWidth, size.height), verticalSplit = true),
          inner(Size(size.width  - leftWidth, size.height), verticalSplit = true)
        )
      }

      def randomOrientationBranch = {
        val (splitVertically, newRng) = RNG.nextBoolean(varRng)
        varRng = newRng

        if (splitVertically)
          verticalBranch
        else
          horizontalBranch
      }

      if(size.surface > minSurface)
        size.shape match {
          case Square if size.width > minEdgeLength * 2 => randomOrientationBranch
          case SkewedHorizontally if size.width > minEdgeLength * 2  => horizontalBranch
          case SkewedVertically if size.height > minEdgeLength * 2 => verticalBranch
        }
      else Leaf(size)
    }


    val (firstSplitIsVertical, newRng) = RNG.nextBoolean(varRng)
    varRng = newRng

    val tree = inner(size, firstSplitIsVertical)
    (tree, varRng)
  }
}