package com.algo.linkedlist

object RemoveNthNode extends App {

   class ListNode(_x: Int = 0, _next: ListNode = null) {
     var next: ListNode = _next
     var x: Int = _x
   }

  object Solution {
    object Solution {
      def removeNthFromEnd(head: ListNode, n: Int): ListNode = {
        @scala.annotation.tailrec
        def countNumOfNodes(node: ListNode, count: Int): Int = {
          if (node == null) {
            count
          } else {
            countNumOfNodes(node.next, count + 1)
          }
        }
        val numOfNodes = countNumOfNodes(head, 0)
        val nodeToRemoveFromFront = numOfNodes - n + 1

        @scala.annotation.tailrec
        def remove(currNode: ListNode, optPrevNode: Option[ListNode], count: Int): ListNode = {
          if(count == nodeToRemoveFromFront) {
            optPrevNode match {
              case Some(prevNode) =>
                prevNode.next = currNode.next
                head
              case None =>
                currNode.next
            }
          } else {
            remove(currNode.next, Option(currNode), count + 1)
          }
        }

        remove(head, None, 1)
      }
    }
  }
}
