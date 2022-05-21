package com.algo.linkedlist

object ReverseLinkedListII extends App {

  class ListNode(_x: Int = 0, _next: ListNode = null) {
    var next: ListNode = _next
    var x: Int = _x
  }

  object Solution {
    def reverseBetween(head: ListNode, left: Int, right: Int): ListNode = {
      @scala.annotation.tailrec
      def reverse(index: Int, currNode: ListNode, tailNode: ListNode): Unit = {
        if(index > right) {
          ()
        } else if (index == left - 1) {
          reverse(index + 1, currNode.next, currNode)
        } else if (index >= left) {
          if(index + 1 <= right) {
            val temp = currNode.next.next
            tailNode.next = currNode.next
            tailNode.next.next = currNode
            currNode.next = temp
            reverse(index + 2, temp, tailNode)
          } else if (index <= right) {

          }
        } else {
          reverse(index + 1, currNode.next, tailNode)
        }
      }

      val dummyNode = new ListNode(0)
      dummyNode.next = head
      reverse(0, dummyNode, dummyNode)
      dummyNode.next
    }
  }
}
