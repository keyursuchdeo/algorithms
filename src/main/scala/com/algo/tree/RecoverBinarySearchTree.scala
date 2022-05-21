package com.algo.tree

object RecoverBinarySearchTree extends App {

  class TreeNode(_value: Int = 0, _left: TreeNode = null, _right: TreeNode = null) {
    var value: Int = _value
    var left: TreeNode = _left
    var right: TreeNode = _right
  }

  object Solution {
    def recoverTree(root: TreeNode): Unit = {
      def recover(currNode: TreeNode, lowerLimitNode: TreeNode, upperLimitNode: TreeNode): Boolean = {
        if(currNode == null) {
          false
        } else if(currNode.value > lowerLimitNode.value && currNode.value < upperLimitNode.value) {
          val swapCorrected = recover(currNode.left, lowerLimitNode, currNode)
          if(swapCorrected) {
            swapCorrected
          } else {
            recover(currNode.right, currNode, upperLimitNode)
          }
        } else if (currNode.value > lowerLimitNode.value) {
          if(upperLimitNode.right != null && upperLimitNode.right.value < currNode.value) {
            val temp = upperLimitNode.right.value
            upperLimitNode.right.value = currNode.value
            currNode.value = temp
            true
          } else {
            val temp = currNode.value
            currNode.value = upperLimitNode.value
            upperLimitNode.value = temp
            true
          }
        } else if (currNode.value < upperLimitNode.value) {
          if(lowerLimitNode.right != null && lowerLimitNode.right.value < currNode.value) {
            val temp = lowerLimitNode.right.value
            lowerLimitNode.right.value = currNode.value
            currNode.value = temp
            true
          } else {
            val temp = currNode.value
            currNode.value = lowerLimitNode.value
            lowerLimitNode.value = temp
            true
          }
        } else {
          true
        }
      }

      val swapCorrected = recover(root.left, new TreeNode(Int.MinValue), root)
      if(swapCorrected) {
        ()
      } else {
        recover(root.right, root, new TreeNode(Int.MaxValue))
        ()
      }
    }
  }
}
