package com.algo.arrays

object UniqueBinarySearchTreesII extends App {

  class TreeNode(_value: Int = 0, _left: TreeNode = null, _right: TreeNode = null) {
    var value: Int = _value
    var left: TreeNode = _left
    var right: TreeNode = _right
  }

  object Solution {
    def treeNode(n: Int): TreeNode = {
      new TreeNode(n)
    }

    def generateTrees(n: Int): List[TreeNode] = {
      def generate(currN: Int): List[TreeNode] = {
        if (currN > n || currN < 1) {
          Nil
        } else if (currN == 1) {
          List(treeNode(currN))
        } else {
          val currHead = treeNode(currN)
          if (currN == n) {
            generate(currN - 1).map(tree => {
              currHead.left = tree
              currHead
            })
          } else {
            generate(currN - 1).map(tree => {
              currHead.left = tree
              currHead
            }) ++
            generate(currN + 1).map(tree => {
              currHead.right = tree
              currHead
            })
          }
        }
      }

      generate(n)
    }
  }
}
