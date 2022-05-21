package com.algo.arrays

object UniqueBinarySearchTreesII2 extends App {

  case class TreeNode(_value: Int = 0, _left: TreeNode = null, _right: TreeNode = null) {
    var value: Int = _value
    var left: TreeNode = _left
    var right: TreeNode = _right
  }

  val out = Solution.generateTrees(4)
  println(out)

  object Solution {
    def treeNode(n: Int): TreeNode = {
      new TreeNode(n)
    }

    def generateTrees(n: Int): List[TreeNode] = {
      def generate(low: Int, high: Int): List[TreeNode] = {
        if (high < low) {
          Nil
        } else if (high == low) {
          List(treeNode(high))
        } else {
          (low to high).flatMap(num => {
            val left = generate(low, num - 1)
            val right = generate(num + 1, high)
            if (left.isEmpty) {
              right.map(r => {
                val head = treeNode(num)
                head.right = r
                head
              })
            } else if (right.isEmpty) {
              left.map(l => {
                val head = treeNode(num)
                head.left = l
                head
              })
            } else {
              for {
                l <- left
                r <- right
              } yield {
                val head = treeNode(num)
                head.left = l
                head.right = r
                head
              }
            }
          }).toList
        }
      }

      generate(1, n)
    }
  }
}
