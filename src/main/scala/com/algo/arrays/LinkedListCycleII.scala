package com.algo.arrays

object LinkedListCycleII extends App {

  class ListNode(var _x: Int = 0) {
    var next: ListNode = null
    var x: Int = _x
  }


  object Solution {
    def detectCycle(head: ListNode): ListNode = {

      @scala.annotation.tailrec
      def isCyclePresent(slow: ListNode, fast: ListNode): (Boolean, ListNode) = {
        if(slow == fast) {
          (true, fast)
        } else if(fast == null || fast.next == null) {
          (false, fast)
        } else {
          isCyclePresent(slow.next, fast.next.next)
        }
      }

      @scala.annotation.tailrec
      def findCycleStartNode(slow: ListNode, fast: ListNode): ListNode = {
        if(slow == fast) {
          slow
        } else {
          findCycleStartNode(slow.next, fast.next)
        }
      }

      if(head == null || head.next == null) {
        null
      } else {
        val (cyclePresent, fastNode) = isCyclePresent(head.next, head.next.next)
        if(cyclePresent) {
          findCycleStartNode(head, fastNode)
        } else {
          null
        }
      }
    }
  }
}
