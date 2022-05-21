package com.algo.dp

import scala.collection.mutable

object BinTreeMaxPathSum extends App {

  class TreeNode(_value: Int = 0, _left: TreeNode = null, _right: TreeNode = null) {
    var value: Int = _value
    var left: TreeNode = _left
    var right: TreeNode = _right
  }

  //  val root = new TreeNode(-10)
  //  root.left = new TreeNode(9)
  //  root.right = new TreeNode(20)
  //  root.right.left = new TreeNode(15)
  //  root.right.right = new TreeNode(7)

  val root = new TreeNode(1)
  root.left = new TreeNode(2)
  root.left.left = new TreeNode(3)
  root.left.left.left = new TreeNode(4)
  root.left.left.left.left = new TreeNode(5)

  val res = Solution.maxPathSum(root)
  println(res)

  object Solution {
    def maxPathSum(root: TreeNode): Int = {
      val queue: mutable.Queue[Option[TreeNode]] = new mutable.Queue[Option[TreeNode]]()

      @scala.annotation.tailrec
      def treeToSeq(seq: Seq[Option[Int]]): Seq[Option[Int]] = {
        if (queue.isEmpty) {
          seq
        } else {
          val optNode = queue.dequeue()
          optNode match {
            case Some(node) =>
              queue.enqueue(Option(node.left))
              queue.enqueue(Option(node.right))
              treeToSeq(Option(node.value) +: seq)
            case None =>
              treeToSeq(None +: seq)
          }
        }
      }

      queue.enqueue(Option(root))
      val s = treeToSeq(Nil)
      val nodes = s.reverse
      val numOfNodes = nodes.length

      val array = Array.fill[Option[Int]](numOfNodes)(None)

      def calculateSum(index: Int): Int = {
        if (index >= numOfNodes) {
          0
        } else {
          array(index) match {
            case Some(sum) => sum
            case None =>
              nodes(index) match {
                case Some(value) =>
                  val v =
                    Math.max(
                      Math.max(
                        value + calculateSum(index * 2 + 1) + calculateSum(index * 2 + 2),
                        value + calculateSum(index * 2 + 1)
                      ),
                      value + calculateSum(index * 2 + 2)
                    )
                  array(index) = Option(Math.max(value, v))
                  v
                case None =>
                  array(index) = None
                  0
              }
          }
        }
      }

      calculateSum(0)
      println(array.mkString(","))
      println(s)
      array.flatten.max
    }
  }

}
