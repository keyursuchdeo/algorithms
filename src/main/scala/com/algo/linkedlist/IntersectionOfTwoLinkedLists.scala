package com.algo.linkedlist

object IntersectionOfTwoLinkedLists extends App {

  class ListNode(var _x: Int = 0) {
    var next: ListNode = null
    var x: Int = _x
  }


  object Solution {
    def getIntersectionNode(headA: ListNode, headB: ListNode): ListNode = {

      @scala.annotation.tailrec
      def find(currNodeA: ListNode, currNodeB: ListNode): ListNode = {
        if (currNodeA == currNodeB) {
          currNodeA
        } else {
          val updatedNodeA = if(currNodeA == null) headB else currNodeA.next
          val updatedNodeB = if(currNodeB == null) headA else currNodeB.next
          find(updatedNodeA, updatedNodeB)
        }
      }

      find(headA, headB)
    }
  }

}
