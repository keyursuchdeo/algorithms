package com.algo.tree

object BalancedBinaryTree extends App {

  class TreeNode(_value: Int = 0, _left: TreeNode = null, _right: TreeNode = null) {
    var value: Int = _value
    var left: TreeNode = _left
    var right: TreeNode = _right
  }

  object Solution {
    def isBalanced(root: TreeNode): Boolean = {


      def fillDiffBetLeftRightSubTreeHeight(currNode: TreeNode): Int = {
        if(currNode == null) {
          0
        } else {
          val leftHeight = 1 + fillDiffBetLeftRightSubTreeHeight(currNode.left)
          val rightHeight = 1 + fillDiffBetLeftRightSubTreeHeight(currNode.right)
          currNode.value = Math.abs(leftHeight - rightHeight)
          Math.max(leftHeight, rightHeight)
        }
      }

      def check(currNode: TreeNode): Boolean = {
        if(currNode == null) {
          true
        } else {
          currNode.value <= 1 && check(currNode.left) && check(currNode.right)
        }
      }

      fillDiffBetLeftRightSubTreeHeight(root)
      check(root)
    }
  }

}
