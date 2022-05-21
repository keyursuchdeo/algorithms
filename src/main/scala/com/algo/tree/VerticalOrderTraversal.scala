package com.algo.tree

import scala.collection.mutable

object VerticalOrderTraversal extends App {

  class TreeNode(_value: Int = 0, _left: TreeNode = null, _right: TreeNode = null) {
    var value: Int = _value
    var left: TreeNode = _left
    var right: TreeNode = _right
  }

  object Solution {
    def verticalTraversal(root: TreeNode): List[List[Int]] = {
      var map = mutable.Map[Int, List[(Int, Int)]]()

      def traverse(node: TreeNode, nodeCoordinateX: Int, nodeCoordinateY: Int): Unit = {
        if (node == null) {
          ()
        } else {
          val updatedValues =
            map.get(nodeCoordinateX) match {
              case None =>
                List((nodeCoordinateY, node.value))
              case Some(values) =>
                ((nodeCoordinateY, node.value) +: values)
            }
          map = map + (nodeCoordinateX -> updatedValues)
          traverse(node.left, nodeCoordinateX - 1, nodeCoordinateY - 1)
          traverse(node.right, nodeCoordinateX + 1, nodeCoordinateY - 1)
        }
      }

      if (root == null) Nil else {
        traverse(root, 0, 0)
        val min = map.minBy(_._1)._1
        val max = map.maxBy(_._1)._1
        (min to max).foldLeft[List[List[Int]]](Nil)((b, a) => map(a).sortBy(_._1).map(_._2) +: b)
      }
    }
  }

}
