package com.algo.bfs

object HouseRobberIII extends App {

  class TreeNode(_value: Int = 0, _left: TreeNode = null, _right: TreeNode = null) {
    var value: Int = _value
    var left: TreeNode = _left
    var right: TreeNode = _right
  }

  val root = new TreeNode(11)
  root.left = new TreeNode(100)
  root.right = new TreeNode(5)
  root.left.left = new TreeNode(7)

  val res = Solution.rob(root)
  println(res)

  object Solution {
    def rob(root: TreeNode): Int = {
      def calculate(currLeftNode: TreeNode, currRightNode: TreeNode, parentNode: TreeNode, gpIncluded: Boolean): Int = {
        if (currLeftNode == null && currRightNode == null) {
          0
        }  else if (currLeftNode == null) {
          if(gpIncluded) {
            Math.max(
              calculate(currRightNode.left, currRightNode.right, currRightNode, gpIncluded = false),
              currRightNode.value
            )
          } else {
            Math.max(
              parentNode.value + calculate(currRightNode.left, currRightNode.right, currRightNode, gpIncluded = true),
              currRightNode.value
            )
          }
        } else if (currRightNode == null) {
          if(gpIncluded) {
            Math.max(
              calculate(currLeftNode.left, currLeftNode.right, currLeftNode, gpIncluded = false),
              currLeftNode.value
            )
          } else {
            Math.max(
              parentNode.value + calculate(currLeftNode.left, currLeftNode.right, currLeftNode, gpIncluded = true),
              currLeftNode.value
            )
          }
        } else {
          Math.max(
            Math.max(
              Math.max(
                parentNode.value +
                  calculate(currLeftNode.left, currLeftNode.right, currLeftNode, gpIncluded = true) +
                  calculate(currRightNode.left, currRightNode.right, currRightNode, gpIncluded = true),
                currLeftNode.value + currRightNode.value),
              calculate(currLeftNode.left, currLeftNode.right, currLeftNode, gpIncluded = false) + currRightNode.value),
            calculate(currRightNode.left, currRightNode.right, currRightNode, gpIncluded = false) + currLeftNode.value)

        }
      }

      calculate(root.left, root.right, root, gpIncluded = false)
    }
  }

}
