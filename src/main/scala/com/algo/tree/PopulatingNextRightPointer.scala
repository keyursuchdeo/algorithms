package com.algo.tree

import scala.collection.mutable

object PopulatingNextRightPointer extends App {

  class Node(var _value: Int) {
    var value: Int = _value
    var left: Node = null
    var right: Node = null
    var next: Node = null
  }

  object Solution {
    def connect(root: Node): Node = {
      @scala.annotation.tailrec
      def connectNextRight(queue: mutable.Queue[(Node, Int)]): Unit = {
        if (queue.isEmpty) {
          ()
        } else {
          val (currNode, currNodeLevel) = queue.dequeue()
          queue.headOption match {
            case Some((nextNode, nextNodeLevel)) if nextNodeLevel == currNodeLevel =>
              currNode.next = nextNode
              if (currNode.left != null && currNode.right != null) {
                queue.enqueue((currNode.left, currNodeLevel + 1))
                queue.enqueue((currNode.right, currNodeLevel + 1))
              }
              connectNextRight(queue)
            case Some(_) =>
              if (currNode.left != null && currNode.right != null) {
                queue.enqueue((currNode.left, currNodeLevel + 1))
                queue.enqueue((currNode.right, currNodeLevel + 1))
              }
              connectNextRight(queue)
            case None =>
              ()
          }
        }
      }
      if(root == null || (root.left == null && root.right == null)) {
        root
      } else {
        val q = new mutable.Queue[(Node, Int)]()
        q.enqueue((root.left, 1))
        q.enqueue((root.right, 1))
        connectNextRight(q)
        root
      }
    }
  }

}
