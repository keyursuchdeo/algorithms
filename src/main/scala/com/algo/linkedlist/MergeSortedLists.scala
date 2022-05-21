package com.algo.linkedlist

import scala.util.Try

object MergeSortedLists extends App {

  val a = Array(1, 4, 5)
  val b = Array(1, 3, 4)
  val c = Array(2, 6)

  val la = new ListNode(1)
  la.next = new ListNode(4)
  la.next.next = new ListNode(5)

  val lb = new ListNode(1)
  lb.next = new ListNode(3)
  lb.next.next = new ListNode(4)

  val lc = new ListNode(2)
  lc.next = new ListNode(6)

  class ListNode(var _x: Int = 0) {
    var next: ListNode = null
    var x: Int = _x

    override def toString: String = {
      s"$x -> ${Try(next.toString).getOrElse("")}"
    }
  }

  val res = Solution.mergeKLists(Array(la, lb, lc))
  println(res)

  object Solution {
    def mergeKLists(lists: Array[ListNode]): ListNode = {
      @scala.annotation.tailrec
      def merge(index: Int, mergedSeq: Seq[Int]): Seq[Int] = {
        if (index == lists.length) {
          mergedSeq
        } else {
          val updatedMergedList = mergeLists(listNodeToSeq(lists(index)), mergedSeq, Nil)
          merge(index + 1, updatedMergedList)
        }
      }

      seqToListNode(merge(0, Nil))
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
    private def mergeLists(list1: Seq[Int], list2: Seq[Int], mergedList: Seq[Int]): Seq[Int] = {
      (list1, list2) match {
        case (x :: xs, y :: ys) =>
          if (x < y) mergeLists(xs, list2, x +: mergedList) else mergeLists(list1, ys, y +: mergedList)
        case (Nil, _) =>
          mergedList.reverse ++ list2
        case (_ , Nil) =>
          mergedList.reverse ++ list1
        case (_, _) =>
          mergedList
      }
    }
  }
}
