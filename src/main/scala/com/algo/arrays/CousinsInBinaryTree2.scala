package com.algo.arrays

object CousinsInBinaryTree2 extends App {

  val root = new TreeNode(1)
  root.left = new TreeNode(2)
//  root.right = new TreeNode(3)
//  root.left.right = new TreeNode(4)
//  root.right.right = new TreeNode(5)

  val res = Solution.isCousins(root, 1, 2)
  println(res)

  class TreeNode(_value: Int = 0, _left: TreeNode = null, _right: TreeNode = null) {
    var value: Int = _value
    var left: TreeNode = _left
    var right: TreeNode = _right
  }

  object Solution {
    def isCousins(root: TreeNode, x: Int, y: Int): Boolean = {
      val xDepthAndParent: Option[(Int, Option[TreeNode])] = depthAndParent(root, None, x, 0)
      val yDepthAndParent = depthAndParent(root, None, y, 0)
      (xDepthAndParent, yDepthAndParent) match {
        case (Some((xDepth, Some(xParent))), Some((yDepth, Some(yParent)))) =>
          xDepth == yDepth && xParent.value != yParent.value
        case _ => false
      }
    }

    def depthAndParent(node: TreeNode, parentNode: Option[TreeNode], value: Int, currDepth: Int): Option[(Int, Option[TreeNode])] = {
      if (node == null) {
        None
      } else if (value == node.value) {
        Option((currDepth, parentNode))
      } else if (Option(value) == Option(node.left).map(_.value) ||
        Option(value) == Option(node.right).map(_.value)) {
        Option((currDepth + 1, Option(node)))
      } else {
        depthAndParent(node.left, Option(node), value, currDepth + 1) match {
          case Some(depth) => Some(depth)
          case _ => depthAndParent(node.right, Option(node), value, currDepth + 1)
        }
      }
    }
  }

}
