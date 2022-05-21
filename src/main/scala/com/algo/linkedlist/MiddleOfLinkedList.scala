package com.algo.linkedlist

object MiddleOfLinkedList extends App {

  class ListNode(_x: Int = 0, _next: ListNode = null) {
    var next: ListNode = _next
    var x: Int = _x
  }

  object Solution {
    def middleNode(head: ListNode): ListNode = {
      @scala.annotation.tailrec
      def find(slow: ListNode, fast: ListNode): ListNode = {
        if(fast == null) {
          slow
        } else {
          if(fast.next == null) {
            slow
          } else {
            find(slow.next, fast.next.next)
          }
        }
      }

      find(head, head.next)
    }
  }
}
