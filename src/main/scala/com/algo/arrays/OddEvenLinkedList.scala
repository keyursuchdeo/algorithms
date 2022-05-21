package com.algo.arrays

object OddEvenLinkedList extends App {

 class ListNode(_x: Int = 0, _next: ListNode = null) {
   var next: ListNode = _next
   var x: Int = _x
 }


  object Solution {
    def oddEvenList(head: ListNode): ListNode = {
      if (head == null) {
        null
      } else if (head.next == null){
        head
      } else {
        val odd: ListNode = new ListNode(head.x)
        val even = new ListNode(head.next.x)

        @scala.annotation.tailrec
        def prepOddEven(next: ListNode, index: Int, prevOddNode: ListNode, prevEvenNode: ListNode): Unit = {
          if (next == null) {
            prevOddNode.next = even
          } else if (index % 2 == 0) {
            val nextOddNode = new ListNode(next.x)
            prevOddNode.next = nextOddNode
            prepOddEven(next.next, index + 1, nextOddNode, prevEvenNode)
          } else {
            val nextEvenNode = new ListNode(next.x)
            prevEvenNode.next = nextEvenNode
            prepOddEven(next.next, index + 1, prevOddNode, nextEvenNode)
          }
        }

        println(head.next.next)
        prepOddEven(head.next.next, 2, odd, even)
        odd
      }
    }
  }
}
