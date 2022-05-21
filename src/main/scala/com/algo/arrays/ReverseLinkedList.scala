package com.algo.arrays

object ReverseLinkedList extends App {

  class ListNode(_x: Int = 0, _next: ListNode = null) {
    var next: ListNode = _next
    var x: Int = _x
  }

  object Solution {
    def reverseList(head: ListNode): ListNode = {
      var newHead: ListNode = null
      def reverse(curr: ListNode, next: ListNode, prev: ListNode): Unit = {
        if(curr == null || curr.next == null) {
          newHead = curr
          ()
        } else {
          val afterNext = curr.next.next
          reverse(next, afterNext, curr)
          next.next = curr
          curr.next = prev
        }
      }

      reverse(head, head.next, null)
      newHead
    }
  }
}
