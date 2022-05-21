package com.algo.arrays

object RemoveDuplicatesSortedList extends App {

  class ListNode(_x: Int = 0, _next: ListNode = null) {
    var next: ListNode = _next
    var x: Int = _x
  }

  object Solution {
    def deleteDuplicates(head: ListNode): ListNode = {
      @scala.annotation.tailrec
      def delete(currNode: ListNode, prevNode: ListNode): Unit = {
        if(currNode == null) {
          ()
        } else {
          if(prevNode == null) {
            val next = currNode.next
            currNode.next = null
            delete(next, currNode)
          } else {
            if(currNode.x == prevNode.x) {
              delete(currNode.next, prevNode)
            } else {
              val next = currNode.next
              currNode.next = null
              prevNode.next = currNode
              delete(next, currNode)
            }
          }
        }
      }

      delete(head, null)
      head
    }
  }
}
