package com.algo.tree

object LevelOrderTraversal extends App {

  class TreeNode(_value: Int = 0, _left: TreeNode = null, _right: TreeNode = null) {
    var value: Int = _value
    var left: TreeNode = _left
    var right: TreeNode = _right
  }

  def levelOrder(root: TreeNode): List[List[Int]] = {
    @scala.annotation.tailrec
    def traverse(nodes: Seq[TreeNode], traversedNodes: Seq[List[Int]]): List[List[Int]] = {
      if (nodes.isEmpty) {
        traversedNodes.toList
      } else {
        val childNodes =
          nodes.flatMap(node => {
            Seq(Option(node.left), Option(node.right)).flatten
          })
        traverse(childNodes, nodes.map(_.value).toList +: traversedNodes)
      }
    }

    traverse(Seq(root), Nil)
  }
}
