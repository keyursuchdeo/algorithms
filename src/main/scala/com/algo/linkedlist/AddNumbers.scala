package com.algo.linkedlist

import scala.util.Try

object AddNumbers extends App {

  class ListNode(var _x: Int = 0) {
    var next: ListNode = null
    var x: Int = _x

    override def toString: String = {
      s"$x -> ${Try(next.toString).getOrElse("")}"
    }
  }

  val num1 = Seq(2, 4, 3)
  val num2 = Seq(5, 6, 4)

  val num1ListNode = new ListNode(2)
  num1ListNode.next = new ListNode(4)
  num1ListNode.next.next = new ListNode(7)

  val num2ListNode = new ListNode(5)
  num2ListNode.next = new ListNode(6)
  num2ListNode.next.next = new ListNode(4)

  val a = Solution.addTwoNumbers(num1ListNode, num2ListNode)
  println(a)

  object Solution {
    def addTwoNumbers(l1: ListNode, l2: ListNode): ListNode = {

      @scala.annotation.tailrec
      def add(dn1:ListNode, dn2: ListNode, carry: Int, seq: Seq[Int]): (Seq[Int]) = {
        if (dn1 == null && dn2 == null) {
          if(carry == 0) {
            seq
          } else {
            carry +: seq
          }
        } else if (dn1 == null) {
          val (num, newCarry) = numAndCarry(dn2.x + carry)
          add(null, dn2.next, newCarry, num +: seq)
        } else if (dn2 == null) {
          val (num, newCarry) = numAndCarry(dn1.x + carry)
          add(dn1.next, null, newCarry, num +: seq)
        } else {
          val (num, newCarry) = numAndCarry(dn1.x + dn2.x + carry)
          add(dn1.next, dn2.next, newCarry, num +: seq)
        }
      }

      def numAndCarry(sum: Int): (Int, Int) =
        if (sum >= 10) (sum - 10, 1) else (sum, 0)

      def prepListNode(result: Seq[Int], len: Int): ListNode = {
        val reversed = result.reverse
        var listNodes: Seq[ListNode] = reversed.map(d => new ListNode(d))
        @scala.annotation.tailrec
        def loop(index: Int): Unit = {
          if (index == len - 1) {
            Unit
          } else {
            listNodes(index).next = listNodes(index + 1)
            loop(index + 1)
          }
        }
        loop(0)
        listNodes.head
      }

      val seq = add(l1, l2, 0, Nil)
      prepListNode(seq, seq.length)
    }
  }
}
