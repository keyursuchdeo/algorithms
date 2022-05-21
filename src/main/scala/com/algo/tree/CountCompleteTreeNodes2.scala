package com.algo.tree

object CountCompleteTreeNodes2 extends App {

  class TreeNode(_value: Int = 0, _left: TreeNode = null, _right: TreeNode = null) {
    var value: Int = _value
    var left: TreeNode = _left
    var right: TreeNode = _right
  }

  object Solution {
    def countNodes(root: TreeNode): Int = {
      val leftHeight = countLeft(root)
      val rightHeight = countRight(root)
      if (leftHeight == rightHeight) {
        (1 << leftHeight) - 1
      } else {
        1 + countNodes(root.left) + countNodes(root.right)
      }
    }

    def countLeft(node: TreeNode): Int = {
      if (node == null) 0 else 1 + countLeft(node.left)
    }

    def countRight(node: TreeNode): Int = {
      if (node == null) 0 else 1 + countRight(node.right)
    }
  }

}
