package com.algo.tree

import scala.collection.mutable

object BinaryTreeRightSideView extends App {

  class TreeNode(_value: Int = 0, _left: TreeNode = null, _right: TreeNode = null) {
    var value: Int = _value
    var left: TreeNode = _left
    var right: TreeNode = _right
  }

  object Solution {
    def rightSideView(root: TreeNode): List[Int] = {

      @scala.annotation.tailrec
      def build(queue: mutable.Queue[(TreeNode, Int)], rightMostElement: Int, rightMostElementLevel: Int, view: List[Int]): List[Int] = {
        if(queue.isEmpty) {
          (rightMostElement +: view).reverse
        } else {
          val (head, headLevel) = queue.dequeue()
          if(head.left != null) {
            queue.enqueue((head.left, headLevel + 1))
          }
          if(head.right != null) {
            queue.enqueue((head.right, headLevel + 1))
          }
          if(headLevel == rightMostElementLevel) {
            build(queue, head.value, headLevel, view)
          } else {
            build(queue, head.value, headLevel, rightMostElement +: view)
          }
        }
      }

      if(root == null) {
        Nil
      } else {
        val queue = new mutable.Queue[(TreeNode, Int)]()
        if(root.left != null) {
          queue.enqueue((root.left, 1))
        }
        if(root.right != null) {
          queue.enqueue((root.right, 1))
        }
        build(queue, root.value, 0, Nil)
      }
    }
  }
}
