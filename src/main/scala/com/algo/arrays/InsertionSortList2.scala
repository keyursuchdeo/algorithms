package com.algo.arrays

object InsertionSortList2 extends App {

  class ListNode(_x: Int = 0, _next: ListNode = null) {
    var next: ListNode = _next
    var x: Int = _x
  }

  object Solution {
    def insertionSortList(head: ListNode): ListNode = {

      def insertNodeInto(node: ListNode, currHead: ListNode): ListNode = {
        @scala.annotation.tailrec
        def insertAt(currNode: ListNode, prev: ListNode): Unit = {
          if(currNode == null) {
            prev.next = node
          } else if (node.x < currNode.x) {
            prev.next = node
            node.next = currNode
          } else {
            insertAt(currNode.next, currNode)
          }
        }
        if(node.x < currHead.x) {
          node.next = currHead
          node
        } else {
          insertAt(currHead.next, currHead)
          currHead
        }
      }

      @scala.annotation.tailrec
      def sort(currNode: ListNode, resultingListHead: ListNode): ListNode= {
        if(currNode == null) {
          resultingListHead
        } else {
          if(resultingListHead == null) {
            val currNext = currNode.next
            currNode.next = null
            sort(currNext, currNode)
          } else {
            val unsorted = currNode.next
            val output = insertNodeInto(currNode, resultingListHead)
            sort(unsorted, output)
          }
        }
      }

      sort(head, null)
    }
  }

}
