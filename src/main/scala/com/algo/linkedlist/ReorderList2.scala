package com.algo.linkedlist

object ReorderList2 extends App {

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
      def reorder(node: ListNode): Unit = {
        val (lastNode, optPrevNode) = findLastAndItsPrevNode(node, None)
        optPrevNode match {
          case None => ()
          case Some(prevNode) if prevNode == node => ()
          case Some(prevNode) =>
            prevNode.next = null
            val currNext = node.next
            node.next = lastNode
            lastNode.next = currNext
            reorder(currNext)
        }
      }

      @scala.annotation.tailrec
      def findLastAndItsPrevNode(node: ListNode, prevNode: Option[ListNode]): (ListNode, Option[ListNode]) = {
        if(node.next == null){
          (node, prevNode)
        } else {
          findLastAndItsPrevNode(node.next, Option(node))
        }
      }

      if (head == null) () else reorder(head)
    }
  }

}
