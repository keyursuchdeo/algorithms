package com.algo.tree

object BinaryTreeTilt extends App {

  class TreeNode(_value: Int = 0, _left: TreeNode = null, _right: TreeNode = null) {
    var value: Int = _value
    var left: TreeNode = _left
    var right: TreeNode = _right
  }

  object Solution {

    def findTilt(root: TreeNode): Int = {

      var tilts = Seq[Int]()

      def sumOfTree(node: TreeNode): Int = {
        if(node == null) {
          0
        } else {
          val leftSum = sumOfTree(node.left)
          val rightSum = sumOfTree(node.right)
          tilts = Math.abs(rightSum - leftSum) +: tilts
          node.value +  leftSum + rightSum
        }
      }

      sumOfTree(root)
      tilts.sum

    }
  }
}
