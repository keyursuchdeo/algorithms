package com.algo.tree

object PseudoPalindromicPaths extends App {

  class TreeNode(_value: Int = 0, _left: TreeNode = null, _right: TreeNode = null) {
    var value: Int = _value
    var left: TreeNode = _left
    var right: TreeNode = _right
  }

  object Solution {
    def pseudoPalindromicPaths (root: TreeNode): Int = {

      def isPalindromePossible(numFrequencies: Int): Boolean = {
        (numFrequencies & (numFrequencies - 1)) == 0
      }

      def isLeafNode(currNode: TreeNode): Boolean = currNode.left == null && currNode.right == null

      def find(currNode: TreeNode, numFrequencies: Int): Int = {
        if(currNode == null) {
          0
        } else if(isLeafNode(currNode)) {
          if(isPalindromePossible(numFrequencies ^ (1 << currNode.value))) {
            1
          } else {
            0
          }
        } else {
          find(currNode.left, numFrequencies ^ (1 << currNode.value)) +
            find(currNode.right, numFrequencies ^ (1 << currNode.value))
        }
      }

      find(root, 0)
    }
  }
}
