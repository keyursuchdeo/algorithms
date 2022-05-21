package com.algo.tree

object RecoverBinarySearchTree3 extends App {

  class TreeNode(_value: Int = 0, _left: TreeNode = null, _right: TreeNode = null) {
    var value: Int = _value
    var left: TreeNode = _left
    var right: TreeNode = _right
  }

  object Solution {
    var prev: TreeNode = null
    var first: TreeNode = null
    var last: TreeNode = null
    def recoverTree(root: TreeNode): Unit = {
      def inOrderTraversal(node: TreeNode): Unit = {
        if(node == null) {
          ()
        } else {
          inOrderTraversal(node.left)
          prev = node
          if(first == null && prev != null && prev.value > node.value) {
            first = prev
          }
          if(first != null && prev != null && prev.value > node.value) {
            last = node
          }
          inOrderTraversal(node.right)
        }
      }

      inOrderTraversal(root)
      val temp = first.value
      first.value = last.value
      last.value = temp
    }
  }
}
