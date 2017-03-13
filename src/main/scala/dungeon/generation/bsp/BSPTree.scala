

package dungeon.generation.bsp

import math._

sealed trait BSPTree {
  import BSPTree._

  def positionedLeaves(gridSize: Size): Set[PositionedLeaf] = {

    def inner(tree: BSPTree, position: Position, size: Size): Set[PositionedLeaf] = tree match {
      case VerticalBranch(ratio, top, bottom) =>
        val (topSize, bottomSize) = size.partitionVertically(ratio)
        inner(top, position, topSize) ++
        inner(bottom, position.down(topSize.height), bottomSize)

      case HorizontalBranch(ratio, left, right) =>
        val (leftSize, rightSize) = size.partitionHorizontally(ratio)
        inner(left, position, leftSize) ++
        inner(right, position.right(leftSize.width), rightSize)

      case Leaf => Set(PositionedLeaf(position, size))
    }

    inner(this, Position(0,0), gridSize)
  }

}



object BSPTree {
  case class VerticalBranch(ratio: Double, top: BSPTree, bottom: BSPTree) extends BSPTree
  case class HorizontalBranch(ratio: Double, left: BSPTree, right: BSPTree) extends BSPTree
  case object Leaf extends BSPTree

  case class PositionedLeaf(position: Position, size: Size)

  def buildRandomTree(maxLeafSurface: Double, minLeafSurface: Double, skewdnessCutoff: Double)(rng: RNG): (BSPTree, RNG) = {
    var varRng: RNG = rng


    def inner(size: RatioSize, verticalSplit: Boolean): BSPTree = {

      def verticalBranch = {
        val (ratioOfSplit, newRng) = RNG.nextGaussianRatio(4)(varRng)
        varRng = newRng
        VerticalBranch(
          ratioOfSplit,
          inner(RatioSize(size.width, size.height * ratioOfSplit), verticalSplit = false),
          inner(RatioSize(size.width, size.height * (1 - ratioOfSplit)), verticalSplit = false)
        )
      }

      def horizontalBranch = {
        val (ratioOfSplit, newRng) = RNG.nextGaussianRatio(4)(varRng)
        varRng = newRng
        HorizontalBranch(
          ratioOfSplit,
          inner(RatioSize(size.width * ratioOfSplit, size.height), verticalSplit = true),
          inner(RatioSize(size.width * (1 - ratioOfSplit), size.height), verticalSplit = true)
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

      size.shape(skewdnessCutoff) match {
        case Normal if size.surfaceArea > maxLeafSurface => randomOrientationBranch
        case Normal if size.surfaceArea < minLeafSurface => Leaf
        case Normal =>
          val (stopSplitting, newRng) = RNG.nextBoolean(varRng)
          varRng = newRng

          if(stopSplitting) Leaf
          else randomOrientationBranch

        case SkewedHorizontally => horizontalBranch
        case SkewedVertically => verticalBranch
      }
    }


    val (firstSplitIsVertical, newRng) = RNG.nextBoolean(varRng)
    varRng = newRng

    val tree = inner(RatioSize(1.0, 1.0), firstSplitIsVertical)
    (tree, varRng)
  }
}