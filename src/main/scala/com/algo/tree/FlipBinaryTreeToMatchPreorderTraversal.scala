package com.algo.tree

object FlipBinaryTreeToMatchPreorderTraversal extends App {

  class TreeNode(_value: Int = 0, _left: TreeNode = null, _right: TreeNode = null) {
    var value: Int = _value
    var left: TreeNode = _left
    var right: TreeNode = _right
  }

  object Solution {
    def flipMatchVoyage(root: TreeNode, voyage: Array[Int]): List[Int] = {
      var flips: Seq[Int] = Nil
      var index: Int = 0

      def dfs(currNode: TreeNode): Unit = {
        if(currNode == null) {
          ()
        } else {
          if(currNode.value == voyage(index)) {
            index = index + 1
            if(currNode.left != null && currNode.left.value == voyage(index + 1)) {
              dfs(currNode.left)
              dfs(currNode.right)
            } else if (currNode.left == null) {
              dfs(currNode.right)
            } else {
              flips = currNode.value +: flips
              dfs(currNode.right)
              dfs(currNode.left)
            }
          } else {
            flips = Seq(-1)
          }
        }
      }

      dfs(root)
      flips.toList
    }
  }
}
