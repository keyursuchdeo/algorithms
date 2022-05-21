package com.algo.arrays

object RotateList extends App {

  class ListNode(_x: Int = 0, _next: ListNode = null) {
    var next: ListNode = _next
    var x: Int = _x
  }

  object Solution {
    def rotateRight(head: ListNode, k: Int): ListNode = {
      @scala.annotation.tailrec
      def calculateLength(currNode: ListNode, length: Int): Int = {
        if (currNode == null) {
          length
        } else {
          calculateLength(currNode.next, length + 1)
        }
      }

      @scala.annotation.tailrec
      def getLastNode(currNode: ListNode, prevNode: ListNode): ListNode = {
        if(currNode == null) {
          prevNode
        } else {
          getLastNode(currNode.next, currNode)
        }
      }

      @scala.annotation.tailrec
      def rotate(prevNode: ListNode, currNode: ListNode, currLength: Int, lengthAtTheCut: Int): ListNode = {
        if (currLength == lengthAtTheCut) {
          if(prevNode == null) {
            currNode
          } else {
            prevNode.next = null
            val lastNode = getLastNode(currNode, null)
            lastNode.next = head
            currNode
          }
        } else {
          rotate(currNode, currNode.next, currLength + 1, lengthAtTheCut)
        }
      }

      if(k == 0) {
        head
      } else {
        val length = calculateLength(head, 0)
        if(k == length) {
          head
        } else {
          val rotationLength = k % length
          rotate(null, head, 0, length - rotationLength)
        }
      }
    }
  }

}
