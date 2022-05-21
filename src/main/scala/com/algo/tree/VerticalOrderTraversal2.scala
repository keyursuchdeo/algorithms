package com.algo.tree

import scala.collection.mutable

object VerticalOrderTraversal2 extends App {

  class TreeNode(_value: Int = 0, _left: TreeNode = null, _right: TreeNode = null) {
    var value: Int = _value
    var left: TreeNode = _left
    var right: TreeNode = _right
  }

  object Solution {
    def verticalTraversal(root: TreeNode): List[List[Int]] = {
      var map = mutable.Map[(Int, Int), List[Int]]()

      object CoordinateOrder extends Ordering[(Int, Int)] {
        override def compare(c1: (Int, Int), c2: (Int, Int)): Int = {
          val (x1, y1) = c1
          val (x2, y2) = c2

          if(x1 == x2) {
            y2 compare y1
          } else {
            x1 compare x2
          }
        }
      }

      def traverse(node: TreeNode, nodeCoordinateX: Int, nodeCoordinateY: Int): Unit = {
        if (node == null) {
          ()
        } else {
          val updatedValues =
            map.get((nodeCoordinateX, nodeCoordinateY)) match {
              case None =>
                List(node.value)
              case Some(values) =>
                (node.value +: values)
            }
          map = map + ((nodeCoordinateX, nodeCoordinateY) -> updatedValues)
          traverse(node.left, nodeCoordinateX - 1, nodeCoordinateY - 1)
          traverse(node.right, nodeCoordinateX + 1, nodeCoordinateY - 1)
        }
      }

      def prepOutput(input: List[((Int, Int), List[Int])]): List[List[Int]] = {
        @scala.annotation.tailrec
        def prep(prev: ((Int, Int), List[Int]), currList: List[((Int, Int), List[Int])], currOutput: List[Int], output: List[List[Int]]): List[List[Int]] = {
          if(currList.isEmpty) {
            currOutput +: output
          } else {
            val ((x, _), headList) = currList.head
            val ((prevX, _), prevList) = prev
            if(x == prevX) {
              prep(currList.head, currList.tail, prevList ++ headList, output)
            } else {
              prep(currList.head, currList.tail, headList, prevList +: output)
            }
          }
        }


        input.headOption match {
          case None => Nil
          case Some(head) =>
            prep(head, input.tail, head._2, Nil)
        }
      }

      if (root == null) Nil else {
        traverse(root, 0, 0)
        prepOutput(map.toList.sortBy(_._1)(CoordinateOrder)).reverse
      }
    }
  }

}
