package com.algo.linkedlist

import scala.collection.mutable
import scala.util.Try

object ReverseNodes extends App {

  val a = Array(1, 2, 3, 4, 5)

  val la = new ListNode(1)
  la.next = new ListNode(2)
  la.next.next = new ListNode(3)
  la.next.next.next = new ListNode(4)
  la.next.next.next.next = new ListNode(5)

  val res = Solution.reverseKGroup(null, 0)
  println(res)

  class ListNode(var _x: Int = 0) {
    var next: ListNode = null
    var x: Int = _x

    override def toString: String = {
      s"$x -> ${Try(next.toString).getOrElse("")}"
    }
  }

  object Solution {
    def reverseKGroup(head: ListNode, k: Int): ListNode = {
      if (head == null) null else {
        val seq = listNodeToSeq(head)
        val reversedSeq: Seq[Seq[Int]] = accKNodesAndReverse(k, seq, Nil)
        val reversedReversedSeq = reversedSeq.reverse
        println(reversedReversedSeq)
        seqToListNode(reversedReversedSeq.flatten)
      }
    }

    private def listNodeToSeq(listNode: ListNode): Seq[Int] = {
      @scala.annotation.tailrec
      def prepSeq(node: ListNode, seq: Seq[Int]): Seq[Int] = {
        if (node == null) {
          seq.reverse
        } else {
          prepSeq(node.next, node.x +: seq)
        }
      }

      prepSeq(listNode, Nil)
    }

    private def seqToListNode(seq: Seq[Int]) = {
      val len = seq.length
      val listNodes = seq.map(num => new ListNode(num))
      @scala.annotation.tailrec
      def loop(index: Int): Unit = {
        if (index >= len - 1) {
          ()
        } else {
          listNodes(index).next = listNodes(index + 1)
          loop(index + 1)
        }
      }
      loop(0)
      listNodes.headOption.orNull
    }

    @scala.annotation.tailrec
    private def accKNodesAndReverse(k: Int, seq: Seq[Int], reversedSeqs: Seq[Seq[Int]]): Seq[Seq[Int]] = {
      val (accNums, accCount, remainingNums) = accumulate(k, seq)
      if (accCount < k) {
        seq +: reversedSeqs
      } else {
        val reversedSeq: Seq[Int] = reverse(accNums, accCount)
        accKNodesAndReverse(k, remainingNums, reversedSeq +: reversedSeqs)
      }
    }

    private def reverse(accNums: mutable.Queue[Int], accCount: Int): Seq[Int] = {
      @scala.annotation.tailrec
      def rev(count: Int, reversedSeq: Seq[Int]): Seq[Int] = {
        if (count <= 0) {
          reversedSeq
        } else {
          rev(count - 1, accNums.dequeue() +: reversedSeq)
        }
      }

      rev(accCount, Nil)
    }

    private def accumulate(k: Int, seq: Seq[Int]): (mutable.Queue[Int], Int, Seq[Int]) = {
      val accNums: mutable.Queue[Int] = new mutable.Queue[Int]

      @scala.annotation.tailrec
      def acc(accCount: Int, remainingNums: Seq[Int]): (Int, Seq[Int]) = {
        (accCount, remainingNums) match {
          case (count, x :: xs) if count < k =>
            accNums.enqueue(x)
            acc(accCount + 1, xs)
          case (count, _) if count >= k => (count, remainingNums)
          case (count, Nil) => (count, remainingNums)
        }
      }

      val (accCount, remainingNums) = acc(0,seq)
      (accNums, accCount, remainingNums)
    }
  }
}
