package com.algo.dp

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

object BinTreeMaxPathSum2 extends App {

  class TreeNode(_value: Int = 0, _left: TreeNode = null, _right: TreeNode = null) {
    var value: Int = _value
    var left: TreeNode = _left
    var right: TreeNode = _right
  }

//    val root = new TreeNode(-10)
//    root.left = new TreeNode(9)
//    root.right = new TreeNode(20)
//    root.right.left = new TreeNode(15)
//    root.right.right = new TreeNode(7)

//  val root = new TreeNode(1)
//  root.left = new TreeNode(2)
//  root.left.left = new TreeNode(3)
//  root.left.left.left = new TreeNode(4)
//  root.left.left.left.left = new TreeNode(5)

  val root = new TreeNode(5)
  root.left = new TreeNode(4)
  root.right = new TreeNode(8)
  root.left.left = new TreeNode(11)
  root.right.left = new TreeNode(13)
  root.right.right = new TreeNode(4)



  val res = Solution.maxPathSum(root)
  println(res)

  object Solution {
    def maxPathSum(root: TreeNode): Int = {
      var ans: Int = Int.MinValue
      def sum(node: TreeNode): Int= {
        if(node == null) {
          0
        } else {
          val x = sum(node.left)
          val y = sum(node.right)
          ans = Math.max(ans, x + y + node.value)
          Math.max(0, node.value + Math.max(x, y))
        }
      }

      sum(root)
      println(ans)
      ans
    }
  }

}
