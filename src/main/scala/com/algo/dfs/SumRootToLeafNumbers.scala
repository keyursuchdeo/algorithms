package com.algo.dfs

object SumRootToLeafNumbers extends App {

  class TreeNode(_value: Int = 0, _left: TreeNode = null, _right: TreeNode = null) {
    var value: Int = _value
    var left: TreeNode = _left
    var right: TreeNode = _right
  }

//  val r = new TreeNode(1)
//  r.left = new TreeNode(2)
//  r.right = new TreeNode(3)

//  val r = new TreeNode(4)
//  r.left = new TreeNode(9)
//  r.right = new TreeNode(0)
//  r.left.left = new TreeNode(5)
//  r.left.right = new TreeNode(1)

  val r = new TreeNode(4)
  r.right = new TreeNode(0)

  val res = Solution.sumNumbers(r)
  println(res)

  object Solution {
    def sumNumbers(root: TreeNode): Int = {
      @scala.annotation.tailrec
      def dfs(node: TreeNode, currNum: String, stack: Seq[(TreeNode, String)], nums: Seq[Int]): Int = {
        if(node == null) {
          if(stack.isEmpty) {
            (currNum.toInt +: nums).sum
          } else {
            val poppedNode = stack.head
            dfs(poppedNode._1, poppedNode._2, stack.tail, currNum.toInt +: nums)
          }
        } else {
          if(node.right != null) {
            val currValue = currNum + node.value
            if(node.left == null) {
              dfs(node.left, "0", (node.right, currValue) +: stack, nums)
            } else {
              dfs(node.left, currValue, (node.right, currValue) +: stack, nums)
            }
          } else {
            dfs(node.left, currNum + node.value, stack, nums)
          }
        }
      }

      dfs(root, "", Nil, Nil)
    }
  }

}
