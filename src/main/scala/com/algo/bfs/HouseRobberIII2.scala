package com.algo.bfs

object HouseRobberIII2 extends App {

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

      var map: Map[(TreeNode, Boolean), Int] = Map[(TreeNode, Boolean), Int]()

      def calculate(currNode: TreeNode, parentIncluded: Boolean): Int = {
        if (currNode == null) {
          0
        } else {
          map.get((currNode, parentIncluded)) match {
            case Some(value) => value
            case None =>
              val value =
                if (parentIncluded) {
                  calculate(currNode.left, parentIncluded = false) +
                    calculate(currNode.right, parentIncluded = false)
                } else {
                  Math.max(
                    currNode.value +
                      calculate(currNode.left, parentIncluded = true) +
                      calculate(currNode.right, parentIncluded = true),
                    calculate(currNode.left, parentIncluded = false) +
                      calculate(currNode.right, parentIncluded = false)
                  )
                }
              map = map + ((currNode, parentIncluded) -> value)
              value
          }
        }
      }

      Math.max(
        calculate(root, parentIncluded = false),
        calculate(root, parentIncluded = true)
      )
    }
  }

}
