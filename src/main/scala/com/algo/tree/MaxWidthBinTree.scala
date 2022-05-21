package com.algo.tree

object MaxWidthBinTree extends App {

  class TreeNode(_value: Int = 0, _left: TreeNode = null, _right: TreeNode = null) {
    var value: Int = _value
    var left: TreeNode = _left
    var right: TreeNode = _right
  }

  object Solution {
    def widthOfBinaryTree(root: TreeNode): Int = {
      @scala.annotation.tailrec
      def calculateMaxWidth(nodes: Seq[(TreeNode, Int)], level: Int, maxWidth: Int): Int = {
        if(nodes.isEmpty) {
          maxWidth
        } else {
          val childNodes: Seq[(TreeNode, Int)] = nodes.flatMap(nodeAndNum => {
            val (node, num) = nodeAndNum
            Seq(Option(node.right).map((_, 2*num + 1)), Option(node.left).map((_, 2*num))).flatten
          })
          if (childNodes.isEmpty) {
            calculateMaxWidth(childNodes, level + 1, maxWidth)
          } else {
            val currWidth = (childNodes.head._2 - (Math.pow(2, level) + 1)).toInt + 1
            println(currWidth)
            val updatedMaxWidth = Math.max(maxWidth, currWidth)
            calculateMaxWidth(childNodes, level + 1, updatedMaxWidth)
          }
        }
      }

      calculateMaxWidth(Seq((root, 1)), 0, 1)
    }
  }

}
