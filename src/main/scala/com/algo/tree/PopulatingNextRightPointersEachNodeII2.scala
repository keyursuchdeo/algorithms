package com.algo.tree

object PopulatingNextRightPointersEachNodeII2 extends App {

  class Node(var _value: Int) {
    var value: Int = _value
    var left: Node = null
    var right: Node = null
    var next: Node = null
  }


  object Solution {
    def connect(root: Node): Node = {
      @scala.annotation.tailrec
      def doConnect(parent: Node, childHead: Node, child: Node): Unit = {
        if(parent == null) {
          if(childHead != null) {
            doConnect(childHead, null, null)
          } else {
            ()
          }
        } else {
          if(childHead == null) {
            if(parent.left != null) {
              doConnect(parent, parent.left, parent.left)
            } else if (parent.right != null) {
              doConnect(parent.next, parent.right, parent.right)
            } else {
              doConnect(parent.next, childHead, child)
            }
          } else {
            if(parent.left != null && parent.left != child) {
              child.next = parent.left
              doConnect(parent, childHead, parent.left)
            } else if(parent.right != null) {
              child.next = parent.right
              doConnect(parent.next, childHead, parent.right)
            } else {
              doConnect(parent.next, childHead, child)
            }
          }
        }
      }

      doConnect(root, null, null  )
      root
    }
  }

}
