package com.algo.arrays

object AddTwoNumbersII extends App {

  class ListNode(_x: Int = 0, _next: ListNode = null) {
    var next: ListNode = _next
    var x: Int = _x
  }

  object Solution {
    def addTwoNumbers(l1: ListNode, l2: ListNode): ListNode = {
      @scala.annotation.tailrec
      def stackDigits(currNode: ListNode, digits: Seq[Int] = Nil): Seq[Int] = {
        if(currNode == null) {
          digits
        } else {
          stackDigits(currNode.next, currNode.x +: digits)
        }
      }

      @scala.annotation.tailrec
      def addDigits(digits1: Seq[Int], digits2: Seq[Int], output: Seq[Int] = Nil, carry: Int = 0): Seq[Int] = {
        println(digits1)
        println(digits2)
        (digits1, digits2) match {
          case (d1 :: xd1, d2 :: xd2) =>
            addDigits(xd1, xd2, ((carry + d1 + d2) % 10) +: output, (carry + d1 + d2) / 10)
          case (Nil, d2 :: xd2) =>
            addDigits(Nil, xd2, ((carry + d2) % 10) +: output, (carry + d2) / 10)
          case (d1 :: xd1, Nil) =>
            addDigits(xd1, Nil, ((carry + d1) % 10) +: output, (carry + d1) / 10)
          case (_, _) =>
            carry +: output
        }
      }

      @scala.annotation.tailrec
      def prepList(digits: Seq[Int], head: ListNode = null, tail: ListNode = null): ListNode = {
        if(digits.isEmpty) {
          head
        } else {
          if(head == null) {
            val node: ListNode = new ListNode(digits.head)
            prepList(digits.tail, node, node)
          } else {
            val node: ListNode = new ListNode(digits.head)
            tail.next = node
            prepList(digits, head, node)
          }
        }
      }

      val outputDigits = addDigits(stackDigits(l1), stackDigits(l2))
      prepList(outputDigits)
    }
  }
}
