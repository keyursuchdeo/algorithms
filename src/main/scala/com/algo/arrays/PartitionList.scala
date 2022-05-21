package com.algo.arrays

object PartitionList extends App {

  class ListNode(_x: Int = 0, _next: ListNode = null) {
    var next: ListNode = _next
    var x: Int = _x
  }

  object Solution {
    def partition(head: ListNode, x: Int): ListNode = {
      @scala.annotation.tailrec
      def partitionList(currNode: ListNode,
                        beforeHead: ListNode = null,
                        beforeLastNode: ListNode = null,
                        afterHead: ListNode = null,
                        afterLastNode: ListNode = null): ListNode = {
        if(currNode == null) {
          if(beforeLastNode != null) {
            beforeLastNode.next = afterHead
            beforeHead
          } else {
            afterHead
          }
        } else {
          val next = currNode.next
          currNode.next = null
          if(currNode.x < x) {
            if(beforeHead == null) {
              partitionList(next, currNode, currNode, afterHead, afterLastNode)
            } else {
              beforeLastNode.next = currNode
              partitionList(next, beforeHead, currNode, afterHead, afterLastNode)
            }
          } else {
            if(afterHead == null) {
              partitionList(next, beforeHead, beforeLastNode, currNode, currNode)
            } else {
              afterLastNode.next = currNode
              partitionList(next, beforeHead, beforeLastNode, afterHead, currNode)
            }
          }
        }
      }

      partitionList(head)
    }
  }
}
