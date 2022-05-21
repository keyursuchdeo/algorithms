package com.algo.bfs

import scala.collection.mutable

object AverageLevelsBinaryTree extends App {

  class TreeNode(_value: Int = 0, _left: TreeNode = null, _right: TreeNode = null) {
    var value: Int = _value
    var left: TreeNode = _left
    var right: TreeNode = _right
  }

  object Solution {
    def averageOfLevels(root: TreeNode): Array[Double] = {
      @scala.annotation.tailrec
      def calculate(queue: mutable.Queue[(TreeNode, Int)], currSum: Double, currCount: Int, currLevel: Int, averages: Seq[Double]): Array[Double] = {
        if(queue.isEmpty) {
          ((currSum / currCount) +: averages).reverse.toArray
        } else {
          val (node, level) = queue.dequeue()
          if (node.left != null) {
            queue.enqueue((node.left, level + 1))
          }
          if (node.right != null) {
            queue.enqueue((node.right, level + 1))
          }
          if (level > currLevel) {
            calculate(queue, node.value, 1, level, ((currSum / currCount.toDouble) +: averages))
          } else {
            calculate(queue, currSum + node.value, currCount + 1, currLevel, averages)
          }
        }
      }

      if(root == null) {
        Array.empty
      } else {
        val q = new mutable.Queue[(TreeNode, Int)]()
        q.enqueue((root, 0))
        calculate(q, 0, 0, 0, Nil)
      }
    }
  }
}
