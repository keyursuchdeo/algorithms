package com.algo.tree

object InverseBinaryTree extends App {

  class TreeNode(_value: Int = 0, _left: TreeNode = null, _right: TreeNode = null) {
    var value: Int = _value
    var left: TreeNode = _left
    var right: TreeNode = _right
  }

  object Solution {
    def invertTree(root: TreeNode): TreeNode = {
      def inverse(node: TreeNode): Unit = {
        if(node != null && (node.left != null || node.right != null)) {
          val currLeft = node.left
          val currRight = node.right
          node.right = currLeft
          node.left = currRight
          inverse(currLeft)
          inverse(currRight)
        }
      }
      inverse(root)
      root
    }
  }

}
