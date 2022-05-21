package com.algo.reservoirsampling

import scala.util.Random

object LinkedListRandomNode extends App {


  class ListNode(_x: Int = 0, _next: ListNode = null) {
    var next: ListNode = _next
    var x: Int = _x
  }

  class Solution(_head: ListNode) {

    /** @param head The linked list's head.
        Note that the head is guaranteed to be not null, so it contains at least one node. */

    /** Returns a random node's value. */
    def getRandom(): Int = {
      @scala.annotation.tailrec
      def find(currNode: ListNode, count: Int, ans: Int): Int = {
        if(currNode == null) {
          ans
        } else {
          if (Math.random() < 1.0 / count) {
            find(currNode.next, count + 1, currNode.x)
          } else {
            find(currNode.next, count + 1, ans)
          }
        }
      }
      find(_head, 1, 0)
    }

  }

  /**
   * Your Solution object will be instantiated and called as such:
   * var obj = new Solution(head)
   * var param_1 = obj.getRandom()
   */

}
