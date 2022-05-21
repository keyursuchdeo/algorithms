package com.algo.tree

object DeepestLeavesSum extends App {

  class TreeNode(_value: Int = 0, _left: TreeNode = null, _right: TreeNode = null) {
    var value: Int = _value
    var left: TreeNode = _left
    var right: TreeNode = _right
  }

  object Solution {
    def deepestLeavesSum(root: TreeNode): Int = {

      def isLeafNode(currNode: TreeNode) = {
        currNode != null && currNode.left == null && currNode.right == null
      }

      var sumAtMaxDepth = 0
      var maxDepth = 0

      def calculate(currNode: TreeNode, currDepth: Int): Unit = {
        if(currNode == null) {
          ()
        } else if(isLeafNode(currNode)) {
          if(currDepth == maxDepth) {
            sumAtMaxDepth = sumAtMaxDepth + currNode.value
          } else if (currDepth > maxDepth) {
            sumAtMaxDepth = currNode.value
            maxDepth = currDepth
          } else {
            ()
          }
        } else {
          calculate(currNode.left, currDepth + 1)
          calculate(currNode.right, currDepth + 1)
        }
      }

      calculate(root, 0)
      sumAtMaxDepth
    }
  }
}
