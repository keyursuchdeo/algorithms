package com.algo.design

import scala.collection.mutable

object MainBSTIterator extends App {

  val root = new TreeNode(7)
  root.left = new TreeNode(3)
  root.right = new TreeNode(15)
  root.right.left = new TreeNode(9)
  root.right.right = new TreeNode(20)

  import com.algo.design.MainBSTIterator.BSTIterator

  val iterator = new MainBSTIterator.BSTIterator(root)
  println(iterator.next()) // return 3
  println(iterator.next()) // return 7
  println(iterator.hasNext()) // return true
  println(iterator.next()) // return 9
  println(iterator.hasNext()) // return true
  println(iterator.next()) // return 15
  println(iterator.hasNext()) // return true
  println(iterator.next()) // return 20
  println(iterator.hasNext()) // return false


  class TreeNode(var _value: Int) {
    var value: Int = _value
    var left: TreeNode = null
    var right: TreeNode = null
  }

  class BSTIterator(_root: TreeNode) {

    private val bstQueue = accNodeValues()

    private def accNodeValues(): mutable.Queue[Int] = {
      val queue = new mutable.Queue[Int]()

      def acc(node: TreeNode): Unit = {
        if (node == null) {
          ()
        } else {
          acc(node.left)
          queue.enqueue(node.value)
          acc(node.right)
        }
      }
      acc(_root)
      queue
    }

    /** @return the next smallest number */
    def next(): Int = {
      if(bstQueue.nonEmpty){
        bstQueue.dequeue()
      } else {
        -1
      }
    }

    /** @return whether we have a next smallest number */
    def hasNext(): Boolean =
      bstQueue.nonEmpty

  }
}
