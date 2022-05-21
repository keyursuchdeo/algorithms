package com.algo.arrays

object RemoveDuplicatesSortedListII extends App {

  class ListNode(_x: Int = 0, _next: ListNode = null) {
    var next: ListNode = _next
    var x: Int = _x
  }

  object Solution {
    def deleteDuplicates(head: ListNode): ListNode = {
      def isDuplicate(currNode: ListNode) = {
        currNode.next != null && currNode.next.x == currNode.x
      }

      def findNextPossibleNonDuplicateNode(currNode: ListNode) = {
        @scala.annotation.tailrec
        def find(value: Int, nextNode: ListNode): ListNode = {
          if(nextNode == null || nextNode.x != value) {
            nextNode
          } else {
            find(value, nextNode.next)
          }
        }

        find(currNode.x, currNode.next)
      }

      @scala.annotation.tailrec
      def delete(currNode: ListNode, prevNode: ListNode, headNode: ListNode): ListNode = {
        if(currNode == null) {
          headNode
        } else {
          if(isDuplicate(currNode)) {
            val next = findNextPossibleNonDuplicateNode(currNode)
            delete(next, prevNode, headNode)
          } else {
            if(headNode == null) {
              val next = currNode.next
              currNode.next = null
              delete(next, currNode, currNode)
            } else {
              val next = currNode.next
              currNode.next = null
              prevNode.next = currNode
              delete(next, currNode, headNode)
            }
          }
        }
      }

      delete(head, null, null )
    }
  }
}
