package com.algo.tree

object TrimBinarySearchTree extends App {

  class TreeNode(_value: Int = 0, _left: TreeNode = null, _right: TreeNode = null) {
    var value: Int = _value
    var left: TreeNode = _left
    var right: TreeNode = _right
  }

  object Solution {
    def trimBST(root: TreeNode, low: Int, high: Int): TreeNode = {
      def trim(currNode: TreeNode, parentNode: TreeNode): Unit = {
        if(currNode == null) {
          ()
        } else if (currNode.value < low) {
          if(parentNode.right == currNode) {
            parentNode.right = null
          }
          parentNode.left = currNode.right
          trim(currNode.right, parentNode)
        } else if (currNode.value > high) {
          if(parentNode.left == currNode) {
            parentNode.left = null
          }
          parentNode.right = currNode.left
          trim(currNode.left, parentNode)
        } else {
          trim(currNode.left, currNode)
          trim(currNode.right, currNode)
        }
      }

      val tempRootNode = new TreeNode()
      trim(root, tempRootNode)
      if(tempRootNode.left != null) {
        tempRootNode.left
      } else if (tempRootNode.right != null) {
        tempRootNode.right
      } else {
        if(root == null || root.value < low || root.value > high) {
          null
        } else {
          root
        }
      }
    }
  }
}
