package com.algo.tree

object BinTreeFromPreInTraversal extends App {

  class TreeNode(_value: Int = 0, _left: TreeNode = null, _right: TreeNode = null) {
    var value: Int = _value
    var left: TreeNode = _left
    var right: TreeNode = _right
  }

  object Solution {
    def buildTree(preorder: Array[Int], inorder: Array[Int]): TreeNode = {
      def build(pre: Array[Int], in: Array[Int]): TreeNode = {
        if (pre.isEmpty) {
          null
        } else {
          val root = new TreeNode(pre.head)
          val (leftIn, rootIndexInIn) = findLeftInOrderElements(root.value, in)
          val rightIn = in.drop(rootIndexInIn)
          val leftPre = pre.tail.diff(rightIn)
          val rightPre = pre.tail.diff(leftIn)
          root.left = build(leftPre, leftIn)
          root.right = build(rightPre, rightIn)
          root
        }
      }

      def findLeftInOrderElements(root: Int, in: Array[Int]): (Array[Int], Int) = {
        @scala.annotation.tailrec
        def find(index: Int, left: Seq[Int]): (Array[Int], Int) = {
          if(in(index) == root) {
            (left.reverse.toArray, index)
          } else {
            find(index + 1, in(index) +: left)
          }
        }
        find(0, Nil)
      }

      build(preorder, inorder)
    }
  }

}
