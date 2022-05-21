package com.algo.dfs

object BstFromPreOrderTraversal extends App {

  val a = Array(8,5,1,7,10,12)
  val res = Solution.bstFromPreorder(a)
  println(res)

  class TreeNode(_value: Int = 0, _left: TreeNode = null, _right: TreeNode = null) {
    var value: Int = _value
    var left: TreeNode = _left
    var right: TreeNode = _right

    override def toString: String = {
      s"$value,${nullOrValue(left)},${nullOrValue(right)}"
    }

    private def nullOrValue(node: TreeNode): String = {
      if(node == null) {
        "null"
      } else {
        s"${node.value}"
      }
    }
  }


  object Solution {
    def bstFromPreorder(preorder: Array[Int]): TreeNode = {
      var index = 0
      def build(min: Int, max: Int): TreeNode = {
        if(index >= preorder.length) {
          null
        } else {
          var node: TreeNode = null
          val key = preorder(index)
          if(key > min && key < max) {
            node = new TreeNode(key)
            index = index + 1
            node.left = build(min, key)
            node.right = build(key, max)
          }
          node
        }
      }
      build(Int.MinValue, Int.MaxValue)
    }
  }

}
