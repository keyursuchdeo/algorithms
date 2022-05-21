package com.algo.tree

object LCABinTree extends App {

  //Definition for a binary tree node.
  class TreeNode(var _value: Int) {
    var value: Int = _value
    var left: TreeNode = null
    var right: TreeNode = null
  }


  object Solution {
    def lowestCommonAncestor(root: TreeNode, p: TreeNode, q: TreeNode): TreeNode = {
      def findAncestor(node: TreeNode, ancestors: Seq[Int], nodeToFind: TreeNode): Seq[Int] = {
        if(node == null) {
          Nil
        }else if(nodeToFind.value == node.value) {
          node.value +: ancestors
        } else {
          findAncestor(node.left, node.value +: ancestors, nodeToFind) match {
            case Nil => findAncestor(node.right, node.value +: ancestors, nodeToFind)
            case xs => xs
          }
        }
      }

      val pAnc = findAncestor(root, Seq(root.value), p)
      val qAnc = findAncestor(root, Seq(root.value), q)

      new TreeNode(pAnc.intersect(qAnc).head)
    }
  }

}
