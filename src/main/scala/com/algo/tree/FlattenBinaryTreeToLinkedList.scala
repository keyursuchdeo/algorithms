package com.algo.tree

object FlattenBinaryTreeToLinkedList extends App {

  class TreeNode(_value: Int = 0, _left: TreeNode = null, _right: TreeNode = null) {
    var value: Int = _value
    var left: TreeNode = _left
    var right: TreeNode = _right
  }

  object Solution {
    def flatten(root: TreeNode): Unit = {
      def isLeafNode(node: TreeNode): Boolean = {
        node != null && node.left == null && node.right == null
      }

      def perform(currNode: TreeNode): TreeNode = {
        if(currNode == null || isLeafNode(currNode)) {
          currNode
        } else {
          val left = currNode.left
          val right = currNode.right
          currNode.left = null
          currNode.right = left
          val node1 = perform(left)
          if (node1 != null) {
            node1.right = right
            val node2 = perform(right)
            if(node2 != null) node2 else node1
          } else {
            currNode.right = right
            val node2 = perform(right)
            if(node2 != null) node2 else currNode
          }
        }
      }

      perform(root)
    }
  }
}
