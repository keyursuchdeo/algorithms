package com.algo.tree

object MaxDiffNodeAncestor extends App {

 class TreeNode(_value: Int = 0, _left: TreeNode = null, _right: TreeNode = null) {
   var value: Int = _value
   var left: TreeNode = _left
   var right: TreeNode = _right
 }

  object Solution {
    def maxAncestorDiff(root: TreeNode): Int = {
      def findMinMax(currNode: TreeNode, currMaxDiff: Int, currMin: Int, currMax: Int): (Int, Int, Int) = {
        if(currNode == null) {
          (currMaxDiff, currMin, currMax)
        } else {
          val diff = Math.max(
            Math.abs(currNode.value - currMin),
            Math.abs(currNode.value - currMax)
          )
          val (updLDiff, updLMin, updLMax) = findMinMax(currNode.left, Math.max(currMaxDiff, diff), Math.min(currMin, currNode.value), Math.max(currMax, currNode.value))
          val (updRDiff, updRMin, updRMax) = findMinMax(currNode.right, Math.max(currMaxDiff, diff), Math.min(currMin, currNode.value), Math.max(currMax, currNode.value))
          (Math.max(updLDiff, updRDiff), Math.min(updLMin, updRMin), Math.max(updRMax, updLMax))
        }
      }

      val (diff, _, _) = findMinMax(root, Int.MinValue, root.value, root.value)
      diff
    }
  }
}
