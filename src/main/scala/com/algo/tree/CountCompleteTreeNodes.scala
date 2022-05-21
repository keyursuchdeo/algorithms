package com.algo.tree

object CountCompleteTreeNodes extends App {

  class TreeNode(_value: Int = 0, _left: TreeNode = null, _right: TreeNode = null) {
    var value: Int = _value
    var left: TreeNode = _left
    var right: TreeNode = _right
  }

  object Solution {
    def countNodes(root: TreeNode): Int = {
      def count(node: TreeNode): Int = {
        if(node == null) {
          0
        } else {
          1 + count(node.left) + count(node.right)
        }
      }

      count(root)
    }
  }
}
