package com.algo.tree

object ConvertSortedListToBinarySearchTree extends App {

 class ListNode(_x: Int = 0, _next: ListNode = null) {
   var next: ListNode = _next
   var x: Int = _x
 }


 class TreeNode(_value: Int = 0, _left: TreeNode = null, _right: TreeNode = null) {
   var value: Int = _value
   var left: TreeNode = _left
   var right: TreeNode = _right
 }

  object Solution {
    def sortedListToBST(head: ListNode): TreeNode = {
      @scala.annotation.tailrec
      def prepArray(currNode: ListNode, seq: Seq[Int]): Array[Int] = {
        if(currNode == null) {
          seq.reverse.toArray
        } else {
          prepArray(currNode.next, currNode.x +: seq)
        }
      }

      val array = prepArray(head, Nil)

      def prepBST(low: Int, high: Int): TreeNode = {
        if(low == high) {
          new TreeNode(array(low))
        } else {
          val mid = (low + high) / 2
          val midRoot = new TreeNode(array(mid))
          midRoot.left = prepBST(low, mid - 1)
          midRoot.right = prepBST(mid + 1, high)
          midRoot
        }
      }

      prepBST(0, array.length - 1)
    }
  }
}
