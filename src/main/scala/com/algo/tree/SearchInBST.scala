package com.algo.tree

object SearchInBST extends App {

  class TreeNode(_value: Int = 0, _left: TreeNode = null, _right: TreeNode = null) {
    var value: Int = _value
    var left: TreeNode = _left
    var right: TreeNode = _right
  }

  val r = new TreeNode(4)
  r.left = new TreeNode(2)
  r.right = new TreeNode(7)
  r.left.left = new TreeNode(1)
  r.left.right = new TreeNode((3))

  val res = Solution.searchBST(r, 2)
  println(res)

  object Solution {

    def searchBST(root: TreeNode, `val`: Int): TreeNode = {

      @scala.annotation.tailrec
      def find(node: TreeNode): TreeNode = {
        if (node == null) {
          null
        } else if (node.value == `val`) {
          node
        } else if (`val` < node.value) {
          find(node.left)
        } else {
          find(node.right)
        }
      }
      find(root)
    }
  }
}
