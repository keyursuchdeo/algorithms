package com.algo.tree

object BinaryTreeCameras extends App {

  class TreeNode(_value: Int = 0, _left: TreeNode = null, _right: TreeNode = null) {
    var value: Int = _value
    var left: TreeNode = _left
    var right: TreeNode = _right
  }

  object Solution {
    def minCameraCover(root: TreeNode): Int = {
      def isLeafNode(currNode: TreeNode): Boolean = {
        currNode != null && currNode.left == null && currNode.right == null
      }
      def calculate(currNode: TreeNode, nodeIsMonitored: Boolean): Int = {
        if(currNode == null) {
          0
        } else if (isLeafNode(currNode) && nodeIsMonitored) {
          0
        } else if (isLeafNode(currNode)) {
          1
        } else {
          if(nodeIsMonitored) {
            Math.min(
              1 + calculate(currNode.left, nodeIsMonitored = true) + calculate(currNode.right, nodeIsMonitored = true),
              calculate(currNode.left, nodeIsMonitored = true) + calculate(currNode.right, nodeIsMonitored = true)
            )
          } else {
            1 + calculate(currNode.left, nodeIsMonitored = true) + calculate(currNode.right, nodeIsMonitored = true)
          }
        }
      }
      if(root == null) {
        0
      } else if(isLeafNode(root)) {
        1
      } else {
        Math.min(
          calculate(root.left, nodeIsMonitored = false) + calculate(root.right, nodeIsMonitored = false),
          1 + calculate(root.left, nodeIsMonitored = true) + calculate(root.right, nodeIsMonitored = true)
        )
      }
    }
  }
}
