package com.algo.tree

import com.algo.tree.Zigzag.Solution.SubTree.Value

object MaxSumBst extends App {

  class TreeNode(var _value: Int) {
    var value: Int = _value
    var left: TreeNode = null
    var right: TreeNode = null
  }

  object Solution {

    object SubTree extends Enumeration {
      type SubTree = Value
      val Left, Right = Value
    }

    import SubTree._

//    def maxSumBST(root: TreeNode): Int = {
//
//    }
  }
}
