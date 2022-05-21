package com.algo.arrays

object CousinsInBinaryTree extends App {

  val root = new TreeNode(1)
  root.left = new TreeNode(2)
  root.right = new TreeNode(3)
  root.left.left = new TreeNode(4)

  val res = Solution.isCousins(root, 4, 3)
  println(res)

  class TreeNode(_value: Int = 0, _left: TreeNode = null, _right: TreeNode = null) {
    var value: Int = _value
    var left: TreeNode = _left
    var right: TreeNode = _right
  }

  object Solution {
    def isCousins(root: TreeNode, x: Int, y: Int): Boolean = {
      if (isValueInRootSubtree(root, x) || isValueInRootSubtree(root, y)) {
        false
      } else {
        (findSubtreeContaining(root.left, x, Option(root)) match {
          case None => false
          case Some(grandParent) =>
            (grandParent.left != null && grandParent.left.value == y) ||
              (grandParent.right != null && grandParent.right.value == y)
        }) ||
          (findSubtreeContaining(root.right, x, Option(root)) match {
            case None => false
            case Some(parent) =>
              (parent.left != null && parent.left.value == y) ||
                (parent.right != null && parent.right.value == y)
          })
      }
    }

    def isValueInRootSubtree(root: TreeNode, value: Int): Boolean = {
      root.value == value ||
        Option(root.left).map(_.value) == Option(value) ||
        Option(root.right).map(_.value) == Option(value)
    }

    def findSubtreeContaining(node: TreeNode, x: Int, grandParentNode: Option[TreeNode]): Option[TreeNode] = {
      if (node == null) {
        None
      } else if ((node.left != null && node.left.value == x)
        || (node.right != null && node.right.value == x)) {
        grandParentNode
      } else {
        findSubtreeContaining(node.left, x, Option(node)) match {
          case Some(subTree) => Some(subTree)
          case _ => findSubtreeContaining(node.right, x, Option(node))
        }
      }
    }
  }

}
