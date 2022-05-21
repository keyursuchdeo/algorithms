package com.algo.linkedlist

object ReorderList extends App {

  val one = new ListNode(1)
  val two = new ListNode(2)
  val three = new ListNode(3)
  val four = new ListNode(4)

  one.next = two
  two.next = three
  three.next = four

  Solution.reorderList(one)

  class ListNode(_x: Int = 0, _next: ListNode = null) {
    var next: ListNode = _next
    var x: Int = _x
  }

  object Solution {
    def reorderList(head: ListNode): Unit = {

      @scala.annotation.tailrec
      def prepReversedList(node: ListNode, reversedNodes: Seq[ListNode], count: Int): (Seq[ListNode], Int) = {
        if (node == null) {
          (reversedNodes, count)
        } else {
          prepReversedList(node.next, node +: reversedNodes, count + 1)
        }
      }

      @scala.annotation.tailrec
      def reorder(node: ListNode, reversedList: Seq[ListNode], totalNodes: Int, currCount: Int): Unit = {
        if(currCount < Math.ceil(totalNodes / 2)) {
          val currNext = node.next
          node.next = reversedList.head
          node.next.next = currNext
          reorder(currNext, reversedList.tail, totalNodes, currCount + 1)
        } else {
          ()
        }

      }

      val (reversedNode, count) = prepReversedList(head, Nil, 0)
      reorder(head, reversedNode, count, 0)
    }
  }

}
