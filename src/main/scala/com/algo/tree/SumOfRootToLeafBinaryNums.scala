package com.algo.tree

object SumOfRootToLeafBinaryNums extends App {

  class TreeNode(_value: Int = 0, _left: TreeNode = null, _right: TreeNode = null) {
    var value: Int = _value
    var left: TreeNode = _left
    var right: TreeNode = _right
  }

  object Solution {
    def sumRootToLeaf(root: TreeNode): Int = {
      def generateBinNums(node: TreeNode): Seq[String] = {
        if(node == null) {
          Nil
        } else if (node.left == null && node.right == null) {
          Seq(node.value.toString)
        } else {
          generateBinNums(node.left).map(_ + node.value) ++ generateBinNums(node.right).map(_ + node.value)
        }
      }

      def binToInt(bin: String): Int = {
        def binChars = bin.toCharArray
        @scala.annotation.tailrec
        def convert(index: Int, currInt: Int): Int = {
          if(index == binChars.length) {
            currInt
          } else {
            convert(index + 1, currInt + (binChars(index).toString.toInt * Math.pow(2, index)).toInt)
          }
        }
        convert(0, 0)
      }

      val binNums = generateBinNums(root)
      println(binNums)
      println(binNums.map(binToInt))
      1
    }
  }

}
