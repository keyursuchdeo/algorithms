package com.algo.arrays

import java.util

object RussianDollEnvelopes extends App {

  object Solution {
    def maxEnvelopes(envelopes: Array[Array[Int]]): Int = {
      object EnvelopeOrder extends Ordering[Array[Int]] {
        override def compare(x: Array[Int], y: Array[Int]): Int = {
          val Array(xw, xh) = x
          val Array(yw, yh) = y
          if (xw == yw) {
            xh compare yh
          } else {
            xw compare yw
          }
        }
      }

      val sortedEnvelopes = envelopes.sorted(EnvelopeOrder)
      val maxEnvelopesCount = Array.fill[Int](envelopes.length)(1)

      def isRussianDoll(smallerIndex: Int, largerIndex: Int): Boolean = {
        val Array(pw, ph) = sortedEnvelopes(smallerIndex)
        val Array(cw, ch) = sortedEnvelopes(largerIndex)
        cw > pw && ch > ph
      }


      @scala.annotation.tailrec
      def fillMaxEnvelopesCount(index: Int = 1): Unit = {
        if (index == envelopes.length) {
          ()
        } else {
          (0 until index).foreach(i => {
            if(isRussianDoll(i, index)) {
              maxEnvelopesCount(index) = Math.max(maxEnvelopesCount(index), maxEnvelopesCount(i) + 1)
            }
          })
          fillMaxEnvelopesCount(index + 1)
        }
      }

      println(sortedEnvelopes.map(_.mkString(",")).mkString("|"))
      fillMaxEnvelopesCount()
      maxEnvelopesCount.max
    }
  }

}
