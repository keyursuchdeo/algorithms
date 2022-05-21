package com.algo.arrays

object SortList2 extends App {

  class ListNode(_x: Int = 0, _next: ListNode = null) {
    var next: ListNode = _next
    var x: Int = _x

    override def toString: String = {
      if(next == null) {
        s"$x"
      } else {
        s"$x -> ${next.toString}"
      }
    }
  }

  val head = new ListNode(2)
  head.next = new ListNode(1)
  head.next.next = new ListNode(3)
  head.next.next.next = new ListNode(4)

  val res = Solution.sortList(head)
  println(res)

  object Solution {
    def sortList(head: ListNode): ListNode = {

      def split(currHead: ListNode): ListNode = {
        if(currHead == null || currHead.next == null) {
          currHead
        } else {
          val (mid, beforeMid) = findMid(currHead)
          if(beforeMid != null) {
            beforeMid.next = null
            val left = split(currHead)
            val right = split(mid)
            mergeNodes(left, right)
          } else {
            val right = split(mid)
            mergeNodes(null, right)

          }
        }
      }

      def mergeNodes(left: ListNode, right: ListNode): ListNode = {
        val currHead = new ListNode()
        @scala.annotation.tailrec
        def merge(currLeft: ListNode, currRight: ListNode, currTail: ListNode): Unit = {
          if(currLeft == null && currRight == null) {
            ()
          } else if (currLeft == null) {
            currTail.next = currRight
          } else if (currRight == null) {
            currTail.next = currLeft
          } else {
            if(currLeft.x <= currRight.x) {
              currTail.next = currLeft
              merge(currLeft.next, currRight, currTail.next)
            } else {
              currTail.next = currRight
              merge(currLeft, currRight.next, currTail.next)
            }
          }
        }

//        println(s"Merging $left and $right")
        merge(left, right, currHead)
        currHead.next
      }


      def findMid(currHead: ListNode): (ListNode, ListNode) = {
        @scala.annotation.tailrec
        def find(fast: ListNode, slow: ListNode, prev: ListNode): (ListNode, ListNode) = {
          if(fast == null || fast.next == null) {
            (slow, prev)
          } else {
            find(fast.next.next, slow.next, slow)
          }
        }

        if(currHead == null || currHead.next == null) {
          (currHead, null)
        } else {
          find(currHead.next.next, currHead.next, currHead)
        }
      }

      split(head)
    }

  }

}
