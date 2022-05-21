package com.algo.dfs

object KthSmallestBst extends App {

  class TreeNode(_value: Int = 0, _left: TreeNode = null, _right: TreeNode = null) {
    var value: Int = _value
    var left: TreeNode = _left
    var right: TreeNode = _right
  }

//  val r = new TreeNode(5)
//  r.left = new TreeNode(3)
//  r.right = new TreeNode(6)
//  r.left.left = new TreeNode(2)
//  r.left.right = new TreeNode(4)
//  r.left.left.left = new TreeNode(1)

  val r = new TreeNode(5)
  r.right = new TreeNode(8)
  r.right.left = new TreeNode(6)
  r.right.left.right = new TreeNode(7)


  val res = Solution.kthSmallest(r, 4)
  println(res)

  object Solution {
    def kthSmallest(root: TreeNode, k: Int): Int = {
      var stack = Seq[TreeNode]()

      @scala.annotation.tailrec
      def fillNodes(node: TreeNode, nodes: Seq[TreeNode], numOfNodes: Int): Seq[TreeNode] = {
        if ((node == null && stack.isEmpty) || numOfNodes == k) {
          nodes
        } else if (node == null && stack.nonEmpty) {
          val stackHead = stack.head
          stack = stack.tail
          fillNodes(stackHead.right, stackHead +: nodes, numOfNodes + 1)
        } else {
          if (node.left != null) {
            stack = node +: stack
            fillNodes(node.left, nodes, numOfNodes)
          } else if(node.right != null) {
            fillNodes(node.right, node +: nodes, numOfNodes + 1)
          } else {
            fillNodes(null, node +: nodes, numOfNodes + 1)
          }
        }
      }

      fillNodes(root, Nil, 0).head.value
    }
  }
}
