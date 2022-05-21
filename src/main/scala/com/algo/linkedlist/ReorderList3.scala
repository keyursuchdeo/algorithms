package com.algo.linkedlist

object ReorderList3 extends App {

  class ListNode(_x: Int = 0, _next: ListNode = null) {
    var next: ListNode = _next
    var x: Int = _x
  }

  object Solution {
    def reorderList(head: ListNode): Unit = {
      @scala.annotation.tailrec
      def listToStack(currHead: ListNode, stack: Seq[ListNode]): Seq[ListNode]  = {
        if(currHead == null) {
          stack
        } else {
          listToStack(currHead.next, currHead +: stack)
        }
      }

      @scala.annotation.tailrec
      def reorder(currHead: ListNode, stack:  Seq[ListNode]): Unit = {
        if(currHead == null) {
          ()
        } else if (currHead == stack.head) {
          currHead.next = null
        } else {
          val nextHead = currHead.next
          if(nextHead == stack.head) {
            currHead.next = stack.head
            nextHead.next = null
            currHead.next.next = null
          } else {
            currHead.next = stack.head
            currHead.next.next = nextHead
            reorder(nextHead, stack.tail)
          }
        }
      }

      reorder(head, listToStack(head, Nil))
    }
  }

}
