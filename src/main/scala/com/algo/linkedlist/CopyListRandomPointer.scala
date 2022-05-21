package com.algo.linkedlist

object CopyListRandomPointer extends App {

  class Node(var _value: Int) {
    var value: Int = _value
    var next: Node = null
    var random: Node = null
  }


  object Solution {
    def copyRandomList(head: Node): Node = {
      @scala.annotation.tailrec
      def copy(prevNode: Node, copiedPrevNode: Node, map: Map[Node, Node]): Unit = {
        val currNode = prevNode.next
        if (currNode == null) {
          ()
        } else {
          val copiedNode =
            map.get(currNode) match {
              case Some(node) =>
                node.value = currNode.value
                if(currNode.random == currNode) {
                  node.random = node
                } else if(currNode.random != null) {
                  node.random = map.getOrElse(currNode.random, new Node(currNode.random.value))
                } else {
                  node.random = null
                }
                node
              case None =>
                val node = new Node(currNode.value)
                if(currNode.random == currNode) {
                  node.random = node
                } else if(currNode.random != null) {
                  node.random = map.getOrElse(currNode.random, new Node(currNode.random.value))
                } else {
                  node.random = null
                }
                node
            }

          copiedPrevNode.next = copiedNode
          if(copiedNode.random == null) {
            copy(currNode, copiedNode, map + (currNode -> copiedNode))
          } else {
            copy(currNode, copiedNode, map + (currNode -> copiedNode, currNode.random -> copiedNode.random))
          }
        }
      }

      val pNode = new Node(-1)
      pNode.next = head
      val cpNode = new Node(-1)
      copy(pNode, cpNode, Map())
      cpNode.next
    }
  }

}
