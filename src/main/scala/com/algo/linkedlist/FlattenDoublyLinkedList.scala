package com.algo.linkedlist

object FlattenDoublyLinkedList extends App {

//  val h1 = new Node(1)
//  val h2 = new Node(2)
//  val h3 = new Node(3)
//  val h4 = new Node(4)
//  val h5 = new Node(5)
//  val h6 = new Node(6)
  val h7 = new Node(7)
  val h8 = new Node(8)
  val h9 = new Node(9)
  val h10 = new Node(10)
//  val h11 = new Node(11)
//  val h12 = new Node(12)

//  h1.next = h2
//  h2.prev = h1
//
//  h2.next = h3
//  h3.prev = h2
//
//  h3.next = h4
//  h4.prev = h3
//
//  h4.next = h5
//  h5.prev = h4
//
//  h5.next = h6
//  h6.prev = h5
//
//  h3.child = h7

  h7.next = h8
  h8.prev = h7

  h8.next = h9
  h9.prev = h8

  h9.next = h10
  h10.prev = h9

//  h8.child = h11
//
//  h11.next = h12
//  h12.prev = h11

  val res = Solution.flatten(h7)
//  println(res)


  //Definition for a Node.
  class Node(var _value: Int) {
    var value: Int = _value
    var prev: Node = null
    var next: Node = null
    var child: Node = null
  }


  object Solution {
    def flatten(head: Node): Node = {

      def flattenAll(currNode: Node): Node = {
        if (currNode.child != null) {
          val temp: Node = currNode.next
          currNode.next = currNode.child
          currNode.child.prev = currNode
          val out: Node = flattenAll(currNode.child)
          currNode.child = null
          out.next = temp
          temp.prev = out
          flatten(temp)
        } else {
          if(currNode.next == null) {
            currNode
          } else {
            flattenAll(currNode.next)
          }
        }
      }

      if(head == null) head else {
        val currHead = head
        flattenAll(head)
        currHead
      }
    }
  }

}
