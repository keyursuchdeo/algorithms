package com.algo.tree

object BinTreeFromInPostTraversal extends App {

  class TreeNode(_value: Int = 0, _left: TreeNode = null, _right: TreeNode = null) {
    var value: Int = _value
    var left: TreeNode = _left
    var right: TreeNode = _right
  }

//  val i = Array(9,3,15,20,7)
//  val p = Array(9,15,7,20,3)

  val i = Array(1, 2, 3, 4, 5)
  val p = Array(1,5,4,3,2)

  val res = Solution.buildTree(i, p)
  println(res)

  object Solution {
    def buildTree(inorder: Array[Int], postorder: Array[Int]): TreeNode = {

      def prepLeftRightPostOrder(leftIn: Set[Int], rightIn: Set[Int], currPost: Array[Int]): (Array[Int], Array[Int]) = {
        @scala.annotation.tailrec
        def prep(index: Int, left: Seq[Int], right: Seq[Int]): (Array[Int], Array[Int]) = {
          if(index == currPost.length) {
            (left.reverse.toArray, right.reverse.toArray)
          } else {
            if(leftIn.contains(currPost(index))) {
              prep(index + 1, currPost(index) +: left, right)
            } else {
              prep(index + 1, left, currPost(index) +: right)
            }
          }
        }

        prep(0, Nil, Nil)
      }

      def findIndexOf(element: Int, array: Array[Int]): Int = {
        @scala.annotation.tailrec
        def find(index: Int): Int = {
          if(index == array.length) {
            -1
          } else {
            if(array(index) == element) {
              index
            } else {
              find(index + 1)
            }
          }
        }
        find(0)
      }

      def build(currIn: Array[Int], currPost: Array[Int]): TreeNode = {
        if(currIn.isEmpty) {
          null
        } else if (currIn.length == 1) {
          new TreeNode(currIn.head)
        } else {
          val head = currPost(currPost.length - 1)
          val node = new TreeNode(head)
          val (leftIn, rightIn) = currIn.splitAt(findIndexOf(head, currIn))
          if(leftIn.isEmpty) {
            node.right = build(rightIn.tail, currPost.init)
            node
          } else if (rightIn.tail.isEmpty) {
            node.left = build(leftIn, currPost.init)
            node
          } else {
            val (leftPost, rightPost) = prepLeftRightPostOrder(leftIn.toSet, rightIn.tail.toSet, currPost.init)
            node.left = build(leftIn, leftPost)
            node.right = build(rightIn.tail, rightPost)
            node
          }
        }
      }

      build(inorder, postorder)
    }
  }

}
