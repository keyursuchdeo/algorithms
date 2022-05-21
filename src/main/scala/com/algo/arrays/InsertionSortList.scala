package com.algo.arrays

object InsertionSortList extends App {

  class ListNode(_x: Int = 0, _next: ListNode = null) {
    var next: ListNode = _next
    var x: Int = _x
  }

  object Solution {
    def insertionSortList(head: ListNode): ListNode = {

      @scala.annotation.tailrec
      def prepList(seq: Seq[Int], prevListNode: ListNode, headListNode: ListNode): ListNode = {
        if(seq.isEmpty) {
          headListNode
        } else {
          if(prevListNode == null) {
            val hNode: ListNode = new ListNode(seq.head)
            prepList(seq.tail, hNode, hNode)
          } else {
            val node: ListNode = new ListNode(seq.head)
            prevListNode.next = node
            prepList(seq.tail, node, headListNode)
          }
        }
      }

      @scala.annotation.tailrec
      def sort(currNode: ListNode, prevNodes: Seq[Int], poppedNodes: Seq[Int]): Seq[Int]= {
        if(currNode == null) {
          prevNodes.reverse
        } else {
          if(prevNodes.isEmpty) {
            sort(currNode.next, poppedNodes.reverse ++ (currNode.x +: prevNodes), Nil)
          } else {
            val currHead = prevNodes.head
            if(currNode.x < currHead) {
              sort(currNode, prevNodes.tail, currHead +: poppedNodes)
            } else {
              sort(currNode.next, poppedNodes.reverse ++ (currNode.x +: prevNodes), Nil)
            }
          }
        }
      }

      val sorted = sort(head, Nil, Nil)
      println(sorted)
      prepList(sorted, null, null)
    }
  }

}
