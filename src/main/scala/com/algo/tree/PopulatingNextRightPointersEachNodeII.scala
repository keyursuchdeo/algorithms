package com.algo.tree

object PopulatingNextRightPointersEachNodeII extends App {

  class Node(var _value: Int) {
    var value: Int = _value
    var left: Node = null
    var right: Node = null
    var next: Node = null
  }


  object Solution {
    def connect(root: Node): Node = {
      def doConnect(parent: Node, rightSibling: Node): Unit = {
        val left = parent.left
        val right = parent.right

        if(left != null && right != null) {
          left.next = right
          doConnect(left, right)
          if(rightSibling != null && rightSibling.left != null) {
            right.next = rightSibling.left
            doConnect(right, rightSibling.left)
            rightSibling.left.next = rightSibling.right
            doConnect(rightSibling.left, rightSibling.right)
          } else if (rightSibling != null && rightSibling.right != null) {
            right.next = rightSibling.right
            doConnect(right, rightSibling.right)
            doConnect(rightSibling.right, null)
          } else {
            right.next = null
          }
        } else if (left != null && right == null) {
          if(rightSibling != null && rightSibling.left != null) {
            left.next = rightSibling.left
            doConnect(left, rightSibling.left)
            rightSibling.left.next = rightSibling.right
            doConnect(rightSibling.left, rightSibling.right)
          } else if (rightSibling != null && rightSibling.right != null) {
            left.next = rightSibling.right
            doConnect(left, rightSibling.right)
            doConnect(rightSibling.right, null)
          } else {
            doConnect(left, null)
          }
        } else if (right != null && left == null) {
          if(rightSibling != null && rightSibling.left != null) {
            right.next = rightSibling.left
            doConnect(right, rightSibling.left)
            rightSibling.left.next = rightSibling.right
            doConnect(rightSibling.left, rightSibling.right)
          } else if (rightSibling != null && rightSibling.right != null) {
            right.next = rightSibling.right
            doConnect(right, rightSibling.right)
            doConnect(rightSibling.right, null)
          } else {
            doConnect(right, null)
          }
        } else {
          if(rightSibling != null && rightSibling.left != null) {
            rightSibling.left.next = rightSibling.right
            doConnect(rightSibling.left, rightSibling.right)
          } else if (rightSibling != null && rightSibling.right != null) {
            doConnect(rightSibling.right, null)
          } else {
            ()
          }
        }
      }

      if(root == null) root else {
        doConnect(root, null)
        root
      }
    }
  }

}
