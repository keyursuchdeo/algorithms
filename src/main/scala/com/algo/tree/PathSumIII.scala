package com.algo.tree

object PathSumIII extends App {

  class TreeNode(_value: Int = 0, _left: TreeNode = null, _right: TreeNode = null) {
    var value: Int = _value
    var left: TreeNode = _left
    var right: TreeNode = _right
  }

  object Solution {
    def pathSum(root: TreeNode, sum: Int): Int = {

      var allPaths = Set[Seq[Int]]()

      def findPath(node: TreeNode, currSum: Int, currPath: Seq[Int]): Int = {
        if(node == null) {
          0
        } else if(currSum + node.value == sum && !allPaths.contains((node.value +: currPath))) {
          allPaths = allPaths + (node.value +: currPath)
          println(s"found ${node.value +: currPath}")
          1 +
            findPath(node.left, currSum + node.value, node.value +: currPath) +
            findPath(node.left, 0, Nil) +
            findPath(node.right, currSum + node.value, node.value +: currPath) +
            findPath(node.right, 0, Nil)
        } else {
          println(s"not found ${node.value +: currPath}")
          findPath(node.left, currSum + node.value, node.value +: currPath) +
            findPath(node.left, 0, Nil) +
            findPath(node.right, currSum + node.value, node.value +: currPath) +
            findPath(node.right, 0, Nil)
        }
      }
      findPath(root, 0, Nil)
    }
  }
}
