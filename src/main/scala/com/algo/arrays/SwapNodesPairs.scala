package com.algo.arrays

object SwapNodesPairs extends App {

  class ListNode(_x: Int = 0, _next: ListNode = null) {
    var next: ListNode = _next
    var x: Int = _x
  }

  object Solution {
    def swapPairs(head: ListNode): ListNode = {
      @scala.annotation.tailrec
      def swap(node: ListNode, prev: ListNode): Unit = {
        if(node == null || node.next == null) {
          ()
        } else {
          val first = node
          val second = node.next
          if(second == null) {
            ()
          } else {
            first.next = second.next
            second.next = first
            if(prev != null) {
              prev.next = second
            }
            swap(first.next, first)
          }
        }
      }

      val output = if(head == null || head.next == null) head else head.next
      swap(head, null)
      output
    }
  }
}
