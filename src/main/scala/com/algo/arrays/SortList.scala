package com.algo.arrays

object SortList extends App {

  class ListNode(_x: Int = 0, _next: ListNode = null) {
    var next: ListNode = _next
    var x: Int = _x
  }

  val head = new ListNode(4)
  head.next = new ListNode(2)
  head.next.next = new ListNode(1)
  head.next.next.next = new ListNode(3)

  val res = Solution.sortList(head)

  object Solution {
    def sortList(head: ListNode): ListNode = {

      @scala.annotation.tailrec
      def calculateListSize(currNode: ListNode, currSize: Int): Int = {
        if (currNode == null) {
          currSize
        } else {
          calculateListSize(currNode.next, currSize + 1)
        }
      }

      @scala.annotation.tailrec
      def findMidOfList(totalSize: Int, currSize: Int, currNode: ListNode): ListNode = {
        if(totalSize == 1) {
          currNode
        } else if (totalSize == 0) {
          null
        } else {
          if (currSize < totalSize / 2) {
            findMidOfList(totalSize, currSize + 1, currNode.next)
          } else {
            currNode
          }
        }
      }

      def sort(totalSize: Int, currNode: ListNode): ListNode = {
        if (currNode == null || currNode.next == null) {
          currNode
        } else {
          val midNode = findMidOfList(totalSize, 0, currNode)
          val left = sort(totalSize / 2, currNode)
          val right = sort(totalSize - (totalSize / 2), midNode)
          mergeLists(left, right)
        }
      }

      def mergeLists(list1: ListNode, list2: ListNode): ListNode = {
        var mergedListHead: ListNode = null

        @scala.annotation.tailrec
        def merge(currList1: ListNode, currList2: ListNode, currTail: ListNode): Unit = {
          if (currList1 != null && currList2 != null) {
            if (currList1.x <= currList2.x) {
              currTail.next = currList1
              merge(currList1.next, currList2, currList1)
            } else {
              currTail.next = currList2
              merge(currList1, currList2.next, currList2)
            }
          } else if (currList1 == null) {
            currTail.next = currList2
          } else {
            currTail.next = currList1
          }
        }

        if(list1 == null && list2 == null) {
          null
        } else if (list1 == null) {
          list2
        } else if (list2 == null) {
          list1
        } else {
          if(list1.x <= list2.x) {
            mergedListHead = list1
            merge(list1.next, list2, list1)
          } else {
            mergedListHead = list2
            merge(list1, list2.next, list2)
          }
          mergedListHead
        }
      }

      sort(calculateListSize(head, 0), head)
    }
  }

}
