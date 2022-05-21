package com.algo.tree

object SmallestSubtreeAllDeepestNodes extends App {

  class TreeNode(_value: Int = 0, _left: TreeNode = null, _right: TreeNode = null) {
    var value: Int = _value
    var left: TreeNode = _left
    var right: TreeNode = _right
  }

  object Solution {
    def subtreeWithAllDeepest(root: TreeNode): TreeNode = {

      def isLeafNode(node: TreeNode): Boolean = node.left == null && node.right == null

      def findMaxHeightFrom(node: TreeNode): Int = {
        if(node == null) {
          0
        } else if (isLeafNode(node)) {
          1
        } else {
          1 + Math.max(findMaxHeightFrom(node.left), findMaxHeightFrom(node.right))
        }
      }

      @scala.annotation.tailrec
      def findAt(currNode: TreeNode): TreeNode = {
        if(currNode == null) {
          currNode
        } else if (currNode.left != null && currNode.left != null) {
          val leftHeight = findMaxHeightFrom(currNode.left)
          val rightHeight = findMaxHeightFrom(currNode.right)
          if(leftHeight == rightHeight) {
            currNode
          } else if (leftHeight > rightHeight) {
            findAt(currNode.left)
          } else {
            findAt(currNode.right)
          }
        } else if (currNode.left != null) {
          findAt(currNode.left)
        } else if (currNode.right != null) {
          findAt(currNode.right)
        } else {
          currNode
        }
      }

      findAt(root)
    }
  }
}
