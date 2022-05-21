package com.algo.arrays

object RemoveNthNodeFromEndOfList extends App {

  class ListNode(_x: Int = 0, _next: ListNode = null) {
    var next: ListNode = _next
    var x: Int = _x
  }

  object Solution {
    def removeNthFromEnd(head: ListNode, n: Int): ListNode = {
      @scala.annotation.tailrec
      def movePointerToNthNode(nodeCount: Int, currNode: ListNode): ListNode = {
        if(nodeCount == n) {
          currNode
        } else {
          movePointerToNthNode(nodeCount + 1, currNode.next)
        }
      }

      @scala.annotation.tailrec
      def findPosOfSlowPointer(fast: ListNode, slow: ListNode): ListNode = {
        if(fast.next == null) {
          slow
        } else {
          findPosOfSlowPointer(fast.next, slow.next)
        }
      }

      val fast = movePointerToNthNode(0, head)
      if(fast == null) {
        head.next
      } else {
        val slow = findPosOfSlowPointer(fast, head)
        slow.next = slow.next.next
        head
      }
    }
  }
}
