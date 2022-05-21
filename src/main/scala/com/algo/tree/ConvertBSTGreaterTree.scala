package com.algo.tree

object ConvertBSTGreaterTree extends App {

  class TreeNode(_value: Int = 0, _left: TreeNode = null, _right: TreeNode = null) {
    var value: Int = _value
    var left: TreeNode = _left
    var right: TreeNode = _right
  }

  object Solution {
    def convertBST(root: TreeNode): TreeNode = {

      var sum = 0
      def convert(currNode: TreeNode): Unit = {
        if(currNode != null) {
          convert(currNode.right)
          sum = sum + currNode.value
          currNode.value = sum
          convert(currNode.left)
        }
      }

      convert(root)
      root
    }
  }
}
